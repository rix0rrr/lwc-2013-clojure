(ns ql.editor
  (use seesaw.core
       seesaw.mig))

(defn editor []
  (let [input (text :multi-line? true :wrap-lines? true :tab-size 4)
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

(editor)
