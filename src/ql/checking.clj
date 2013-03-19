(ns ql.checking
  (:use ql.expr
        clojure.test
        clojure.contrib.graph
        [clojure.core.match :only [match]]))

(defn checking-error [msg]
  (throw (Exception. msg)))

(defn- expr-deps [expr]
  "Return a list of variable names referred to in the given expression"
  (cond 
    (seq? expr)      (mapcat expr-deps expr)
    (literal? expr)  []
    (operator? expr) []
    :else            [expr]))

(defn concat-merge [maps]
  (apply merge-with (cons conj maps)))

(defn- element-deps [element group-deps]
  "Determine the edges of the dependency tree of the given element, returning a map of of {name depencency-set}
  
  group-deps are dependencies imposed by containing groups."
  (match [element]

         [['calc  name expr caption]]  {name (into #{} (concat (expr-deps expr) group-deps))}
         [['group expr & subelements]] (let [extra-deps (expr-deps expr)]
                                         (concat-merge (map #(element-deps % (concat group-deps extra-deps)) subelements)))
         [[type   name caption]]       {name (into #{} group-deps)}))


(defn- all-names [nmap]
  (-> #{}
    (into (keys nmap))
    (into (apply concat (map second nmap)))))

  (defn- find-cycle [neighbours]
  "Check if the list of edges contains a cycle and if so, return the set of variable names that trigger a cycle"

  ; FIXME: Check for a -> a dependencies -- aren't caught by the algorithm below :)
  (let [self-dep (first (filter #(contains? (second %) (first %)) neighbours))]
    (if (not (nil? self-dep))
      (second self-dep)
      (let [G (struct directed-graph (all-names neighbours) neighbours)]
        (first (filter #(> (count %) 1) (scc G)))))))

(defn check-form [form]
  "Check the dependencies of the given form and raise an error if there is a cycle in them"
  (let [c (find-cycle (element-deps form []))]
    (if (not (nil? c))
      (checking-error (str "These variables all depend on each other: " (vec c))))))

;---------------------------------------------------------------------------
; Tests

(deftest test-expr-deps
         (is (= ['a 'b 'c] (expr-deps '(+ a b (/ 2 c))))))

(deftest test-element-deps
         (let [expr (vec '(group c
                     [currency a "A"]
                     [currency b "B"]
                     [calc a-plus-b (+ a b) "A + B"]
                     [calc twice-a-plus-b (* 2 a-plus-b) "2 * (A + B)"]))]

           (is (= {'a-plus-b #{'a 'b 'c}, 'twice-a-plus-b #{'a-plus-b 'c}}
                  (element-deps expr [])))))

(deftest test-all-names
         (is (= #{'a 'b 'c} (all-names {'a #{'b}, 'b #{'c}}))))

(deftest test-cycle
         (let [E1 {'a #{'b}, 'b #{'c}, 'c #{'d}}]
           (is (nil? (find-cycle E1))))
         (let [E2 {'a #{'b}, 'b #{'c}, 'c #{'a}}]
           (is (= true (find-cycle E2)))))
