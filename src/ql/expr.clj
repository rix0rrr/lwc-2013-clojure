(ns ql.expr
  (:use clojure.test clojure.pprint
        [clojure.core.match :only [match]]))

(def undefined 'undefined)

(defn undefined? [x]
  (= x undefined))

(defn safe [f]
  "Execute a function, returning undefined if executing the original function produces an exception"
  (fn [& args]
    (try (apply f args)
      (catch Exception e#
        (println e#)
        undefined))))

(defn lift [f]
  "Lift a function to return undefined if any of its arguments are undefined,
  or call the original function otherwise"
  (fn [& args]
    (if (some undefined? args) 
      undefined
      (apply f args))))

(defn lifted-if [c then else]
  (if (true? c) then else))

(def operators
  "Map of operator symbols to actual functions"
  {'+   (safe (lift +))
   '-   (safe (lift -))
   '/   (safe (lift /))
   '=   (safe (lift =))
   '<   (safe (lift <))
   '<=  (safe (lift <=))
   '>   (safe (lift >))
   '>=  (safe (lift >=))
   'and (safe (lift (fn [& xs] (every? true? xs))))
   'or  (safe (lift (fn [& xs] (some true? xs))))
   'not (safe (lift not))
   'if  (safe lifted-if)
   })

(defn operator? [sym]
  "Return whether the given symbol refers to an operator exported to the DSL"
  (contains? operators sym))

(defn var-name? [sym var-names]
  "Return whether the given symbol name refers to a variable name"
  (some #{sym} var-names))

(defn literal? [atom]
  "Return whether the given atom is a string, number or boolean literal"
  (or (string? atom) (char? atom)
      (integer? atom) (float? atom)
      (true? atom) (false? atom)))

(defn extractor [name values]
  "Return the extractor function for the given variable name"
  `(let [x# (~values (quote ~name))]
     (if-not (nil? x#) x#
       (throw (Exception. (str "Could not find " (quote ~name) " in " ~values))))))

(defn expr-fn [expr var-names]
  "Translate an expression into a function of a map, passing the declared variable names"
  (let [mapname (gensym)]

    (declare tr-expr)

    (defn tr-application [expr]
      "Translate a function application"
      (let [head (first expr)
            tail (rest expr)]
        (if (operator? head) `((operators (quote ~head)) ~@(map tr-expr tail))
          (throw (Exception. (str "Unrecognized operator " head))))))

    (defn- tr-expr [expr]
      "Translate parts of an expression"
      (cond 
        (seq? expr) (tr-application expr)
        (literal? expr) expr
        (var-name? expr var-names) (extractor expr mapname)
        :else (throw (Exception. (str "Unrecognized expression " expr)))))

    `(fn [~mapname]
        ~(tr-expr expr))))

;---------------------------------------------------------------------------
; Tests

(deftest undefined-test
  (is (undefined? undefined))
  (is (not (undefined? 3))))

(deftest lift-plus-test
         (let [plus (operators '+)]
           (is (= undefined (plus 3 undefined)))
           (is (= undefined (plus undefined 3)))
           (is (= 7 (plus 4 3)))
           (is (undefined? (plus 4 "bla")))))

(deftest lift-or-test
         (let [orr (operators 'or)]
           (is (= undefined (orr true undefined)))
           (is (= undefined (orr undefined false)))
           (is (= true (orr false true)))))

(deftest function
         (let [f (eval (expr-fn '(+ a b) ['a 'b]))]
           (is (= 3 (f {'a 1 'b 2})))
           (is (undefined? (f {'a 1 'b undefined})))))
