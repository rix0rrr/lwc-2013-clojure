(ns ql.html-viewer
  (:use hiccup.core
       hiccup.form
       hiccup.page
       [clojure.core.match :only [match]]
       [cljs.closure :as closure]))

(defn- expand-anon-fn
  "Compiles a single anonymous function body in a dummy namespace, with a gensym'ed
   name. Separate from the general case because cljs doesn't include cljs.core and
   the goog stuff if a namespace is not specified, and advanced gClosure
   optimization drops top-level anonymous fns."
  [fnbody]
  (when-not (and (seq fnbody)
                 ('#{fn fn*} (first fnbody)))
    (throw (IllegalArgumentException. "Simple ClojureScript views must be an anonymous fn, e.g. (fn [doc] …) or #(...)")))
  (let [namespace (gensym)
        name (with-meta (gensym) {:export true})]
    [{:main (symbol (str namespace) (str name))}
     [(list 'ns namespace)
      (list 'def name fnbody)]]))


(defn- view*
  [options body]
  (let [[options' body] (if (and (list? body) ('#{fn fn*} (first body)))
                          (expand-anon-fn body)
                          [nil (vec body)])
        options (merge {:optimizations :advanced :pretty-print false}
                       options'
                       options)]
    (when-not (:main options)
      (throw (IllegalArgumentException. "Must specify a fully-qualified entry point fn via :main option")))
    (str (closure/build body options)
         "return " (-> options :main namespace) \. (-> options :main name))))


(declare render-group)

(defn form-line [left & right]
  [[:div {:style "float: left; clear: left; width: 400px;"} left]
   [:div {:style "float: left; width: 200px;"} right]])

(defn- render-element [element]
  (match element
         ['boolean  name caption]      (form-line (label name caption) (check-box name))
         ['currency name caption]      (form-line (label name caption) [:span "&euro;"] (text-field name))
         ['calc     name expr caption] (form-line (label name caption) [:div caption] [:div expr])
         ['group    expr & elements]   [[:div expr] [:br] (render-group elements)]
         :else                         (form-line "Unknown element" (str element))))

(defn render-group [group]
  (mapcat render-element group))

(defn render [form]
  "Render the given form to HTML"
  (html5 [:head [:title "FIXME: Form should have a title"]
                [:script (view* {:main 'bla :optimizations :none :pretty-print true} '((defn ^:export alert [x] (js/alert x))))]]
         [:body (render-group form)]))
