(ns ql.questionnaire
  (use ql.ql ql.qls ql.expr ql.gui-viewer clojure.pprint))

(defform box1-house-owning
         [boolean has-sold-house   "Did you sell a house in 2010?"]
         [boolean has-bought-house "Did you buy a house in 2010?"]
         [boolean has-maint-loan   "Did you enter a loan for maintenance/reconstruction?"]
         [group has-sold-house
          [currency selling-price "Price the house was sold for"]
          [currency private-debt  "Private debts for the sold house"]]
         [calc value-residue (- selling-price private-debt) "Value residue"]
         [calc twice (* 2 value-residue) "Twice that"])

(defstyles blue-label
           [has-sold-house {:type :radio-group}]
           [value-residue {:label {:foreground "blue"}}]
           [twice         {:label {:background "green"}}])

(defstyles big-label
           [value-residue {:label {:font {:size 20}}}]
           [selling-price {:widget {:font {:size 24}}}])

#_(defform illegal-cyclic-form
         [group x
          [boolean y "Y?"]]
         [group y
          [boolean x "X?"]])

#_(defn -main []
  (let [renderer (-> gui-renderer
                   (blue-label)
                   (big-label))]
    (box1-house-owning renderer {})))

#_(-main)
