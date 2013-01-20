; Trying to get some feel for matches

(ns ql.testing
  (:use [clojure.core.match :only [match]]))


(match ['foo 'bar]
       ['foo x] x
       :else "Nope")

(let [x [1 2 nil nil nil]]
   (match [x]
     [([1] :seq)]   :a0
     [([1 2] :seq)] :a1
     [([1 2 nil nil nil] :seq)] :a2))

(let [x '[a 2]]
  (match [x]
         [['a 2]] "yeah"
         :else "Wut"))
