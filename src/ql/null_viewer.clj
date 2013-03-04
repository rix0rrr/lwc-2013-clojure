(ns ql.null-viewer
  (:use ql.ql ql.expr clojure.pprint))


(defn empty-widget []
  (reify interactive-widget
    (widget-value! [this val]
      (prn "widget-value! " val))
    (widget-value [this]
      (prn "widget-value")
      undefined)
    (widget-listen [this listener]
      (prn "widget-listen"))
    (widget-panel  [this]
      (prn "widget-panel"))))


(def null-renderer
  (reify form-renderer
    (init [this]
      (prn "init"))

    (new-widget [this type value caption]
      (prn (str "new-widget " type " " value " " caption))
      (empty-widget))

    (new-group [this widgets]
      (prn (str "new-group " widgets))
      (empty-widget))

    (display [this widget]
      (prn (str "display")))))
