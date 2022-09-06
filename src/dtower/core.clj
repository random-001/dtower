(ns dtower.core
  (:require
   [portal.api :as p]))


(add-tap #'p/submit)


(comment
 (def p (p/open))
 (def p (p/open {:launcher :intellij})))

(tap> "Hello portal")
