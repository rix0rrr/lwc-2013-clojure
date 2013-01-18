(defproject ql "0.1.0-SNAPSHOT"
  :description "Questionnaire Language for LWC2013"
  :dependencies [[org.clojure/clojure "1.4.0"]
                 [hiccup "1.0.2"]
                 [org.clojure/core.match "0.2.0-alpha11"]
                 [org.clojure/clojurescript "0.0-1450"]
                 [seesaw "1.4.2"]]
  :profiles {:dev {:dependencies [[vimclojure/server "2.3.6"]]
                   :plugins [[org.clojars.autre/lein-vimclojure "1.0.0"]]}}
  :main ql.questionnaire)
