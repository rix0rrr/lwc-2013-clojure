(ns ql.ql
  (:use [clojure.core.match :only [match]]))


(defn default-value [type]
  (case type
    boolean  false
    currency 0
    (throw (Exception. (str "Unknown type" type)))))

(defn operator? [sym]
  "Return whether the given symbol refers to an operator exported to the DSL

  Currently, the entire Clojure standard library is exported."
  (resolve sym))


(defn extractor [name values]
  "Return the extractor function for the given variable name"
  `(let [x# (~values (quote ~name))]
     (if-not (nil? x#) x#
       (throw (Exception. (str "Could not find " (quote ~name) " in " ~values))))))

(defn tr-expr [expr]
  "Translate every free variable in an expression into a function application"
  (cond 
    (seq? expr) (map tr-expr expr)
    (symbol? expr) (if (operator? expr) expr (extractor expr 'values))
    :else expr))

(defn expr-fn [expression]
  "Translate an expression into a function of a map"
  (list 'fn '[values] (tr-expr expression)))

; This is the unit test! ;)
;(let [form (expr-fn '(- a b))]
;  (println form)
;  (let [ fn (eval form)]
;  (println fn)
;  (fn { 'a 5 'b 2 })))


(defn form-widget [widget]
  "Translate a single widget"
  (match [widget]
         [['calc   name expr caption]] ['calc name (expr-fn expr) caption]
         [['group  expr & subwidgets]] (into ['group (expr-fn expr)] (map form-widget subwidgets))
         :else widget))

(defmacro form [& body]
  "Translate a form definition

  Turn expressions into functions of a map that looks up the
  expression variables in the map."
  `(quote ~(map form-widget body)))
