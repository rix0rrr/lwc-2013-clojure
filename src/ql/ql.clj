(ns ql.ql
  (:use ql.expr
        ql.checking
        clojure.test
        clojure.pprint
        [clojure.core.match :only [match]]))

(defn logic-error [msg]
  (throw (Exception. msg)))

(def types-to-widgets {'currency 'number
                       'string 'text
                       'boolean 'checkbox
                       'calc 'label})

(def default-values {'currency undefined
                     'boolean undefined
                     'calc undefined}) ; FIXME: Do type inference for calc

(defprotocol interactive-widget
  "Protocol for widgets"
  (widget-value! [this val])
  (widget-value  [this])
  (widget-listen [this listener])
  (widget-panel  [this]))

(defprotocol form-renderer
  "Protocol for form renderers"
  (init       [x])
  (new-widget [x name type value caption attributes])
  (new-group  [x widgets])
  (display    [x widget]))

(defprotocol form-element
  "Protocol for form elements"
  (el-value    [this name root])
  (el-value!   [this name value])
  (el-listen   [this listener])
  (el-refresh! [this root])
  (el-widget   [this]))

(defn input-element [name widget]
  (reify form-element

    (el-value [this q-name root]
      (if (= name q-name)
        (widget-value widget)
        nil))

    (el-value! [this q-name value]
      (if (= name q-name) (widget-value! widget value)))

    (el-listen [this listener]
      (widget-listen widget listener))

    (el-refresh! [this root]) ; Nothing

    (el-widget [this] widget)))

(defn make-getter [root]
  "Return a function that, when invoked with a symbol, will call the
  el-value function on the given root object.
  
  The el-value call will return either a value or nil."
  (fn [varname]
    (let [v (el-value root varname root)]
      (if (nil? v) undefined v))))

(defn output-element [name value-function widget]
  (reify form-element

    (el-value [this q-name root]
      (if (= name q-name)
        (value-function (make-getter root))
        nil))

    (el-value! [this q-name value]) ; Computed, so nothing to update

    (el-listen [this listener]) ; Nothing

    (el-refresh! [this root]
      (widget-value! widget (el-value this name root)))

    (el-widget [this] widget)))

(defn group-element [renderer value-function & children]
  (if (some nil? children) (logic-error "A child passed to (group-element) is nil!"))
  (let [widget (new-group renderer (map el-widget children))]
    (reify form-element

      (el-value [this q-name root]
        (let [group-visible? (value-function (make-getter root))]
          (cond 
            ; Value of one of its children, if this group is visible
            group-visible? (first (keep identity (map #(el-value % q-name root) children)))

            ; Otherwise nothing
            :else nil)))

      (el-value! [this q-name val]
        (doseq [child children]
          (el-value! child q-name val)))

      (el-listen [this listener]
        (doseq [child children]
          (el-listen child listener)))

      (el-refresh! [this root]
        (let [group-visible? (value-function (make-getter root))]
          (widget-value! widget group-visible?))
        (doseq [child children]
          (el-refresh! child root)))

      (el-widget [this] widget))))

(defn widget-for-type [type]
  "Return the widget to associate with the given input type, or error out if unknown"
  (if (contains? types-to-widgets type)
    (types-to-widgets type)
    (invalid-ql (str "Unknown input type: " type))))

(defn create-elements [element renderer values]
  "Generate create-widget calls for the given element"
  (match [element]
         [['calc  name expr caption]]  `(output-element '~name
                                                        ~(expr-fn expr (keys values))
                                                        (new-widget ~renderer (quote ~name) '~(widget-for-type 'calc) ~(values name) ~caption {}))
                                                
         [['group expr & subelements]] `(group-element ~renderer
                                                       ~(expr-fn expr (keys values))
                                                       ~@(map #(create-elements % renderer values) subelements))

         [[type   name caption]]      `(input-element '~name
                                                      (new-widget ~renderer (quote ~name) '~(widget-for-type type) ~(values name) ~caption {}))
         :else (invalid-ql (str "Unrecogized QL form: " element))))

(defn variables [element]
  "Return a map of variables to types of the elements in the form"
  (match [element]
         [['group expr & children]] (into {} (map variables children))
         [['calc name _ _]] {name 'calc}
         [[type name _]] {name type}))

(defn defaults [variables]
  "Return a map of name -> default given a map of name -> type"
  (into {} (for [[name type] variables]
             [name (get default-values type undefined)])))

(defmacro defform [name & body]
  "Translate a form definition

  Turns a form definition into a function that will accept a renderer at
  runtime and call that renderer to display the form."
  (let [root-group (into '[group true] body)
        var-types (variables root-group)
        default-values (defaults var-types)
        renderer-sym (gensym)]

    ; Static analysis
    (check-form root-group)

    ; Return the run-time function
    `(defn ~name [~renderer-sym initial-values#]
       (init ~renderer-sym)
       (let [root-el# ~(create-elements root-group renderer-sym default-values)]
         (el-refresh! root-el# root-el#)
         (el-listen root-el# (fn [] (el-refresh! root-el# root-el#)))
         (display ~renderer-sym (el-widget root-el#))))))

;---------------------------------------------------------------------------
; Tests

(deftest test-defaults
         (let [defs (defaults {'foo 'currency})]
           (is (= defs {'foo undefined}))))

;(deftest group-test
;         (let [el (vec '(currency selling-price "Price the house was sold for"))
;               x (create-elements el `null-renderer {})
;               ax (eval x)]
;           (pprint x)
;           (prn ax))
;
;         (let [el (vec '(calc remainder (- hi lo) "Something"))
;               x (create-elements el `null-renderer {'hi 100 'lo 10})
;               ax (eval x)]
;           (pprint x)
;           (prn ax))
;
;         (let [el (vec '(group has-sold-house [currency selling-price "Price the house was sold for"]))
;               x (create-elements el `null-renderer {'has-sold-house false 'selling-price 0})
;               ax (eval x)]
;           (pprint x)
;           (prn ax)))
