(ns ql.gui-viewer
  (:use seesaw.core
        [clojure.core.match :only [match]]))

(declare render-group)

(defn- render-widget [onchange type]
  "Render a single widget of a given type

  Returns a map of { :widgets, :updates }. :widgets is/are the created widgets,
  while :update is a function that should be called with the new value
  of the widget whenever it changed. onchange is a function that will
  be called with the new value of a variable when a widget is updated
  "
  (case type
    boolean (let [w (checkbox)]
               (listen w :action (fn [e] (onchange (config w :selected?))))
               { :widget w
                 :update (fn [x] (config! w :selected? x)) })
    currency (let [w (text)]
                (listen w :action (fn [e] (onchange (config w :text))))
                { :widget w
                  :update (fn [x] (config! w :text x)) })
    calc (let [w (label)]
            { :widget w
              :update (fn [x] (config! w :text x)) })
    (str "Unknown widget" type)))


(defn seqfn [a b]
  "Return a function of one argument that calls two functions of one argument in sequence"
  (fn [x] (a x) (b x)))

(defn- render-element [onchange element]
  "Render a single element

  Returns a map of { :widgets, :updates }. :widgets are the created widget,
  while :update is a function that should be called with the new value
  map whenever it changed. onchange is a function that will
  be called with a value update whenever a widget changes
  "
  (match element
         ['group    expr & elements] (let [ws (render-group onchange element)]
                                       ; FIXME: Use the value of expr to update the visibility of the child widgets
                                       ws)
         ['calc     name expr caption] (let [w (render-widget (fn [value]) type)]
                                         { :widgets [caption (w :widget)]
                                           :update (fn [values] ((w :update) (str expr))) })
         [type      name caption]      (let [w (render-widget (fn [value] (onchange name value)) type)]
                                         { :widgets [caption (w :widget)]
                                           :update (fn [values] ((w :update) (values name))) })))

(defn render-group [onchange group]
  (reduce (fn [w1 w2] { :widgets (concat (w1 :widgets) (w2 :widgets))
                        :update (seqfn (w1 :update) (w2 :update)) })
          { :widgets [] :update identity }
          (map render-element onchange group)))

(defn render [form]
  "Render the given form to a GUI"
  (native!)
  (let [onchange (fn [x])]
    (->
      (frame :title "No name yet"
            :content (grid-panel :columns 2
                                :items (render-group onchange form)))
      pack!
      show!)))
