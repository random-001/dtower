(ns dtower.core
  (:require
   [portal.api :as p]
   [charred.api :as charred]
   [clojure.core.match :refer [match]]
   [differ.core :as differ]
   [malli.core :as m]
   [weavejester.dependency :as dep]
   [sc.api :refer [spy defsc]]))


(add-tap #'p/submit)


(comment
 (def p (p/open))
 (def p (p/open {:launcher :intellij}))

 (charred/read-json "{\"a\": 1, \"b\": 2}" :key-fn keyword)
 (charred/write-json-str {:a 1 :b 2}))

(tap> "Hello DTower")
