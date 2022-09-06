(defproject dtower "0.1.0-SNAPSHOT"

  :dependencies [[org.clojure/clojure "1.11.1"]
                 [com.cnuernber/charred "1.012"]
                 [org.clojure/core.match "1.0.0"]
                 [differ "0.3.3"]
                 [weavejester/dependency "0.2.1"]
                 [metosin/malli "0.8.9"]
                 [vvvvalvalval/scope-capture "0.3.3-s1"]
                 [djblue/portal "0.29.1"]]

  :repl-options {:init-ns dtower.core})
