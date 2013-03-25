(ns ql.qls
  (:use ql.ql))

(defn quote-heads [ls]
  (vec (map (fn [x]
           [`(quote ~(first x)) (second x)]) ls)))

(defmacro defstyles [name & body]
  `(let [control-map# (into {} ~(quote-heads body))]
    (defn ~name [renderer#]
       (reify form-renderer

         (init [x]
           (init renderer#))

         (new-widget [x name# type# value# caption# attributes#]
           (new-widget renderer# name# type# value# caption#
                       (merge-with merge attributes# (get control-map# name# {}))))

           (new-group [x widgets#]
                      (new-group renderer# widgets#))

           (display [x widget#]
                    (display renderer# widget#))))))
