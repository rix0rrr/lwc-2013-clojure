(ns ql.editor
  (use seesaw.core
       clojure.pprint
       seesaw.mig)
  (import java.io.StringWriter))

(defn editor [initial-value]
  (let [input (text :multi-line? true :wrap-lines? true :tab-size 4 :text initial-value)
        status (text :multi-line? true :editable? false :wrap-lines? true)]

    (defn program-text [] (text input))
    (defn set-ok! [] (text! status "OK")
                     (config! status :foreground :green))
    (defn set-error! [err] (text! status err)
                     (config! status :foreground :red))

    (listen input :document (fn [e]
                              (try
                                (load-string (str "(use 'ql.ql 'ql.qls 'ql.gui-viewer 'ql.expr)"
                                                  (program-text)
                                                  "(box1-house-owning gui-renderer {})"))
                                (set-ok!)
                                (catch Exception e
                                  (set-error! (str e))))))

    (->
      (frame :title "QL Live Programming"
             :resizable? true
             :content (mig-panel
                        :constraints ["insets 10" "[500px, al left, fill|300px, fill]", "[400px]"]
                        :items [[input "grow"] [status "grow"]]))
      (pack!)
      (show!))))

(defn pprint-str [x]
  (let [w (StringWriter.)]
    (pprint x w)
    (.toString w)))

(defn -main []
  (editor "(defform box1-house-owning
                  [boolean has-sold-house   \"Did you sell a house in 2010?\"]
                  [boolean has-bought-house \"Did you buy a house in 2010?\"]
                  [boolean has-maint-loan   \"Did you enter a loan for maintenance/reconstruction?\"]
                  [group has-sold-house
                     [currency selling-price \"Price the house was sold for\"]
                     [currency private-debt  \"Private debts for the sold house\"]]
                  [calc value-residue (- selling-price private-debt) \"Value residue\"]
                  [calc twice (* 2 value-residue) \"Twice that\"])"))
