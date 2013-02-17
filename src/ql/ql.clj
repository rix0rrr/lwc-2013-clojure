(ns ql.ql
  (:use [clojure.core.match :only [match]]))

(def types-to-widgets {'currency 'number
                       'string 'text
                       'boolean 'checkbox
                       'calc 'label})

(def default-values {'currency 0
                     'boolean false
                     'calc 0}) ; FIXME: Do type inference for calc

(defprotocol form-renderer
  "Protocol for form renderers"
  (init [x])
  (new-widget [x type caption])
  (new-group [x widgets])
  (display [x widget]))

(defprotocol interactive-widget
  "Protocol for form widgets"
  (set-value [x val])
  (on-change [x listener])
  (get-panel [x]))

(defprotocol form-element
  (update [this trigger values])
  (on-change-all [x listener])
  (get-widget [this]))


(defn operator? [sym]
  "Return whether the given symbol refers to an operator exported to the DSL

  Currently, the entire Clojure standard library is exported."
  (resolve sym))

(defn extractor [name values]
  "Return the extractor function for the given variable name"
  `(let [x# (~values (quote ~name))]
     (if-not (nil? x#) x#
       (throw (Exception. (str "Could not find " (quote ~name) " in " ~values))))))

(defn tr-expr [expr valsym]
  "Translate every free variable in an expression into a function application"
  (cond 
    (seq? expr) (map #(tr-expr % valsym) expr)
    (symbol? expr) (if (operator? expr) expr (extractor expr valsym))
    :else expr))

(defn expr-fn [expression]
  "Translate an expression into a function of a map"
  (let [valsym (gensym)]
    `(fn [~valsym]
       ~(tr-expr expression valsym))))

(defn create-input-widget [renderer type name caption]
  (let [w (new-widget renderer type caption)]
    (reify form-element

      (update [this trigger values]
        (if (not= trigger name)
          (set-value w (values name))))

      (on-change-all [this listener]
        (on-change w (fn [value]
                       (listener name value))))

      (get-widget [this] w))))

(defn create-output-widget [renderer type name caption value-function]
  (let [w (new-widget renderer type caption)
        my-listener (atom (fn [name value]))]
    (reify form-element

      (update [this trigger values]
        (let [new-value (value-function values)]
          (set-value w new-value)
          (if (not= trigger name)
            (@my-listener name new-value))))

      (on-change-all [this listener]
        (reset! my-listener listener))

      (get-widget [this] w))))

(defn create-group-widget [renderer value-function & child-elements]
  (let [w (new-group renderer (map get-widget child-elements))]
    (reify form-element

      (update [this trigger values]
        "Update the visibility of this panel and all children"
        (set-value w (value-function values))
        (doseq [child child-elements]
          (update child trigger values)))

      (on-change-all [this listener]
        "Add the listener to all children"
        (doseq [child child-elements]
          (on-change-all child listener)))

      (get-widget [this] w))))

(defn widget-creators [element renderer]
  "Generate create-widget calls for the given element"
  (match [element]
         [['calc  name expr caption]]  `(create-output-widget ~renderer
                                                              '~(types-to-widgets 'calc)
                                                              '~name ~caption ~(expr-fn expr))
         [['group expr & subelements]] `(create-group-widget ~renderer ~(expr-fn expr) ~@(map #(widget-creators % renderer) subelements))
         [[type   name caption]]       `(create-input-widget ~renderer '~(types-to-widgets type) '~name ~caption)))

(defn variables [element]
  "Return a map of variables to types of the elements in the form"
  (match [element]
         [['group expr & children]] (apply merge (map variables children))
         [[type name _]] {name type}))

(defn defaults [variables]
  "Return a pair sequence of defaults given a map of variables"
  (apply concat (for [[name type] variables] 
                  (if (contains? default-values type)
                    [`(quote ~name) (default-values type)]
                    []))))

(defmacro defform [name & body]
  "Translate a form definition

  Turns a form definition into a function that will accept a renderer
  at runtime and call that renderer to display the form."
  (let [form-body (into '[group true] body)
        vars (variables form-body)
        rendersym (gensym)]
    `(defn ~name [~rendersym]
       (init ~rendersym)
       (let [values#       (atom (hash-map ~@(defaults vars)))        ; Holds the name -> value map
             root-widget# ~(widget-creators form-body rendersym)

             trigger-update# (fn [source#]
                               "Trigger the update function with all current values"
                               (try (update root-widget# source# @values#)
                                 (catch Exception e# (prn e#))))

             update-value# (fn [key# value#]
                             "Update the value map with a new (key, value) pair"
                             (swap! values# conj { key# value# })
                             (trigger-update# key#))]
         (on-change-all root-widget# update-value#)
         (trigger-update# nil)
         (display ~rendersym (get-widget root-widget#))
         ))))
