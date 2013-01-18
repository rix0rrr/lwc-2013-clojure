(ns ql.ql
  (:use [clojure.core.match :only [match]]))

(defn operator? [sym]
  "Return whether the given symbol refers to an operator exported to the DSL

  Currently, the entire Clojure standard library is exported."
  (resolve sym))

(defn tr-expr [expr]
  "Translate every free variable in an expression into a function application"
  (cond 
    (seq? expr) (map tr-expr expr)
    (symbol? expr) (if (operator? expr) expr (list 'values expr))
    :else expr))

(defn expr-fn [expression]
  "Translate an expression into a function of a map"
  `(fn [values] ~(tr-expr expression)))

(defn form-widget [widget]
  "Translate a single widget"
  (match widget
         ['calc   name expr caption] ['calc name (expr-fn expr) caption]
         ['group  expr & subwidgets] ['group     (expr-fn expr) (map form-widget subwidgets)]
         :else widget))

(defmacro form [& body]
  "Translate a form definition

  Turn expressions into functions of a map that looks up the
  expression variables in the map."
  (map form-widget body))
