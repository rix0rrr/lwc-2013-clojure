(ns ql.gui-viewer
  (:use seesaw.core
        seesaw.mig
        ql.ql ql.expr
        [clojure.core.match :only [match]]))

(defn has-focus [target]
  (let [^java.awt.Component w (to-widget target)]
    (.isFocusOwner w)))

(defn- create-widget [widget label value-key event-type getter setter value]
  (let [panel (mig-panel
                :constraints ["insets 3 20 3 20" "[400px, al left|100px, fill]"]
                :items [[label] [widget]])]
    (config! widget value-key (setter value))

    (reify interactive-widget
      (widget-value [this]
        (getter (config widget value-key)))

      (widget-value! [this val]
        (config! widget value-key (setter val)))

      (widget-listen [this listener]
        (listen widget event-type (fn [e] (listener))))

      (widget-panel [this] panel))))


(defn- create-group [child-panels]
  (let [panel (vertical-panel :items (map widget-panel child-panels))]
    (reify interactive-widget
      (widget-value [this]
        undefined) ; Group doesn't have a value

      (widget-value! [this vis?]
        (config! panel :visible? (to-bool vis?)))

      (widget-listen [this listener]) ; Nothing to do

      (widget-panel [this] panel))))

(defn- create-correct-widget [type value caption attributes]
  "Render a single widget of a given type"

  (defn- with-attrs [factory]
    "Instantiate seesaw factory with attributes

    Attributes are a map, converted to a list"
    (apply factory (mapcat identity (get attributes :widget {}))))

  (defn- make-label []
    (let [args (concat [:text caption :h-text-position :left] (mapcat identity (get attributes :label {})))]
      (prn args)
      (apply label args)))

  (case type
    number   (create-widget (with-attrs text)     (make-label) :text :key-released to-int from-int value)
    text     (create-widget (with-attrs text)     (make-label) :text :key-released from-string to-string value)
    checkbox (create-widget (with-attrs checkbox) (make-label) :selected? :action from-bool to-bool value)
    label    (create-widget (with-attrs label)    (make-label) :text :action from-string to-string value)
             (create-widget (with-attrs label)    (label (str "Unknown widget type " type)) :text :key-released from-string to-string value))) ; Event won't ever happen


(defn repack-on-resize [frame]
  "Attach a listener to all children inside this frame, repacking the frame whenever a child gets hidden or shown"
  (let [repack (fn [e] (pack! frame))]
    (doseq [s (select frame [:*])]
      (listen s
              :component-hidden repack
              :component-shown repack)))
  frame)

(def gui-renderer
  (reify form-renderer
    (init [this]
      (native!))

    (new-widget [this name type value caption attributes]
      (create-correct-widget type value caption attributes))

    (new-group [this widgets]
      (create-group widgets))

    (display [this widget]
      (->
        (frame :title "No name yet"
               :resizable? false
               :content (grid-panel :columns 2
                                    :items [(widget-panel widget)]))
        (pack!)
        (repack-on-resize)
        (show!)))))
