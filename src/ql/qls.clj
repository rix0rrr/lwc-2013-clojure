(ns ql.qls
  (:use ql.ql))

(defn quote-heads [ls]
  (vec (map (fn [x]
           [`(quote ~(first x)) (second x)]) ls)))

(defn recursive-merge [a b]
  (merge-with (fn [a b] (if (seq? a) (recursive-merge a b) b)) a b))

(defmacro defstyles [name & body]
  `(let [override-map# (into {} ~(quote-heads body))]
    (defn ~name [renderer#]
       (reify form-renderer

         (init [x]
           (init renderer#))

         (new-widget [x name# value# caption# attributes#]
           (new-widget renderer# name# value# caption#
                       (recursive-merge attributes# (get override-map# name# {}))))

           (new-group [x widgets#]
                      (new-group renderer# widgets#))

           (display [x widget#]
                    (display renderer# widget#))))))
