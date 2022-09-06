(ns dtower.core
  (:require
   [portal.api :as p]))


(def p (p/open))

(add-tap #'p/submit)
