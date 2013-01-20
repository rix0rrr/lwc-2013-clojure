(ns ql.gui-viewer
  (:use seesaw.core
        ql.ql
        [clojure.core.match :only [match]]))

(declare render-group)

(defn to-int [s]
  (if (= s "") 0
    (Integer/parseInt s)))

(defn- render-widget [onchange type]
  "Render a single widget of a given type

  Returns a map of { :widgets, :updates }. :widgets is/are the created widgets,
  while :update is a function that should be called with the new value
  of the widget whenever it changed. onchange is a function that will
  be called with the new value of a variable when a widget is updated
  "
  (case type
    boolean (let [w (checkbox)]
               (listen w :action (fn [e]
                                   (onchange (config w :selected?))))
               { :widget w
                 :update (fn [x]
                           (config! w :selected? x)) })
    currency (let [w (text)]
                (listen w :key-released (fn [e] (onchange (to-int (config w :text)))))
                { :widget w
                  :update (fn [x] (config! w :text (str x))) })
    calc (let [w (label)]
            { :widget w
              :update (fn [x] (config! w :text x)) })
    (str "Unknown widget" type)))


(defn seqfn [a b]
  "Return a function of that calls two functions in sequence"
  (fn [& args] (apply a args) (apply b args)))

(defn- render-element [onchange element]
  "Render a single element

  Returns a map of { :widgets, :updates }. :widgets are the created widget,
  while :update is a function that should be called with the new value
  map whenever it changed. onchange is a function that will
  be called with a value update whenever a widget changes
  "
  (match [element]
         [['group    vis-fn-form & elements]] (let [ws (render-group onchange elements)
                                                    vis-fn (eval vis-fn-form)]
                                                {:widgets (ws :widgets)
                                                 :defaults (apply merge (ws :defaults))
                                                 :update (fn [source values]
                                                           ((ws :update) source values)
                                                           (let [vis? (vis-fn values)]
                                                             (doseq [widget (ws :widgets)]
                                                               (config! widget :visible? vis?))))})

         [['calc     name val-fn-form caption]] (let [w (render-widget (fn [value]) 'calc)
                                                      val-fn (eval val-fn-form)]
                                                  {:widgets  [(label caption) (w :widget)]
                                                   :defaults { name (default-value 'currency) } ; FIXME: Use type inference
                                                   :update (fn [source values]
                                                             (if-not (= name source)
                                                               ; Update the widget, but also trigger an on-change for the newly calculated
                                                               ; value
                                                               (let [new-value (val-fn values)]
                                                                 ((w :update) new-value)
                                                                 (onchange name new-value)))) })

         [[type      name caption]]      (let [w (render-widget (fn [value] (onchange name value)) type)]
                                           { :widgets [(label caption) (w :widget)]
                                             :update  (fn [source values]
                                                        (if-not (= name source)
                                                          ((w :update) (values name))))
                                             :defaults { name (default-value type) } })

         :else { :widgets ["huh" (str element)] :update (fn [source values]) } ))

(defn render-group [onchange group]
  (reduce (fn [w1 w2] { :widgets (concat (w1 :widgets) (w2 :widgets))
                        :update (seqfn (w1 :update) (w2 :update))
                        :defaults (conj (w1 :defaults) (w2 :defaults))
                        })
          { :widgets [] :update (fn [x y]) }
          (map (partial render-element onchange) group)))

(defn render [form]
  "Render the given form to a GUI"
  (native!)
  (let [values    (atom {})           ; Holds the name -> value map
        update-fn (atom (fn [x y]))   ; Holds the update functions of all widgets

        trigger-update (fn [source]
                         (@update-fn source @values))

        update-value (fn [key value]  ; Function to update the value map with a new key-value pair
                       (swap! values conj { key value }))

        render-result (render-group (fn [key value]
                                      (update-value key value)
                                      (trigger-update key))
                                    form)]
    (reset! update-fn (render-result :update))
    (swap! values into (render-result :defaults))
    (trigger-update nil)
    (->
      (frame :title "No name yet"
             :content (grid-panel :columns 2
                                  :items (render-result :widgets)))
      pack!
      show!)))
