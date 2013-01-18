(ns ql.questionnaire
  (use ql.ql ql.gui-viewer))

(def box1-house-owning
  (form 
    [boolean has-sold-house   "Did you sell a house in 2010?"]
    [boolean has-bought-house "Did you buy a house in 2010?"]
    [boolean has-maint-loan   "Did you enter a loan for maintenance/reconstruction?"]
    [group has-sold-house
     [currency selling-price "Price the house was sold for"]
     [currency private-debt  "Private debts for the sold house"]
     [calc value-residue (- selling-price private-debt) "Value residue"]]))

(defn -main []
  (println (render box1-house-owning)))
