(ns ql.questionnaire
  (use ql.ql ql.expr ql.gui-viewer clojure.pprint))

(defform box1-house-owning
    [boolean has-sold-house   "Did you sell a house in 2010?"]
    [boolean has-bought-house "Did you buy a house in 2010?"]
    [boolean has-maint-loan   "Did you enter a loan for maintenance/reconstruction?"]
    [group has-sold-house
     [currency selling-price "Price the house was sold for"]
     [currency private-debt  "Private debts for the sold house"]]
    [calc value-residue (- selling-price private-debt) "Value residue"]
    [calc twice (* 2 value-residue) "Twice that"]
     )

#_(defform illegal-cyclic-form
         [group x
          [boolean y "Y?"]]
         [group y
          [boolean x "X?"]])

(defn -main []
  (box1-house-owning gui-renderer {}))

(-main)
