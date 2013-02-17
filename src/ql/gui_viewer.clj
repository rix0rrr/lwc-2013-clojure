(ns ql.gui-viewer
  (:use seesaw.core
        ql.ql
        [clojure.core.match :only [match]]))

(defn to-int [s]
  (if (= s "") 0
    (Integer/parseInt s)))

(defn from-int [i]
  (str i))

(defn has-focus [target]
  (let [^java.awt.Component w (to-widget target)]
    (.isFocusOwner w)))

(defn- create-gui-widget [factory-fn caption value-key event-type getter setter]
  (let [widget (factory-fn)
        panel (horizontal-panel :items [(label caption) widget])]
    (reify interactive-widget
      (set-value [this val]
        (if (not (has-focus widget))
          (config! widget value-key (setter val))))

      (on-change [this listener]
        (listen widget event-type (fn [e]
                                    (listener (getter (config widget value-key))))))
      (get-panel [this] panel))))

(defn- render-widget [type caption]
  "Render a single widget of a given type"
  (case type
    number   (create-gui-widget text caption :text :key-released to-int from-int)
    text     (create-gui-widget text caption :text :key-released identity identity)
    checkbox (create-gui-widget checkbox caption :selected? :action identity identity)
    label    (create-gui-widget label caption :text :action identity identity)
    (create-gui-widget label (str "Unknown widget type " type) :text :key-released identity identity))) ; Event won't ever happen

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

    (new-widget [this type caption] (render-widget type caption))

    (new-group [this widgets]
      (let [panel (vertical-panel :items (map get-panel widgets))]
        (reify interactive-widget
          (set-value [this vis?] (config! panel :visible? vis?))
          (on-change [this listener])
          (get-panel [this] panel))))

    (display [this widget]
      (->
        (frame :title "No name yet"
               :resizable? false
               :content (grid-panel :columns 2
                                    :items [(get-panel widget)]))
        (pack!)
        (repack-on-resize)
        (show!)))))
