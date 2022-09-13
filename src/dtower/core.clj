(ns dtower.core
  (:require
   [portal.api :as p]
   [charred.api :as charred]
   [clj-http.client :as http]
   [clojure.core.match :refer [match]]
   [differ.core :as differ]
   [malli.core :as m]
   [weavejester.dependency :as dep]
   [sc.api :refer [spy defsc]]
   [clojure.java.io :as io]
   [clojure.string :as str]))


(add-tap #'p/submit)


(comment
 (def p (p/open))
 (def p (p/open {:launcher :intellij}))

 (charred/read-json "{\"a\": 1, \"b\": 2}" :key-fn keyword)
 (charred/write-json-str {:a 1 :b 2}))



(def test-req
  "{
    \"towerId\": \"3334478b-6c0d-4ab5-aefc-abc99e9bf623\",
    \"letters\": [
      {
        \"name\": \"К\",
        \"x\": 0,
        \"y\": 0,
        \"z\": 0
      },
      {
        \"name\": \"О\",
        \"x\": 1,
        \"y\": 0,
        \"z\": 0
      },
      {
        \"name\": \"Т\",
        \"x\": 2,
        \"y\": 0,
        \"z\": 0
      }
    ]
  }")

(comment
 (def resp
   (http/post "https://dtower-api.datsteam.dev/towers"
              {:throw-entire-message? true
               :body                  test-req
               :headers               {"Content-Type" "application/json"
                                       "token"        "cuKVhArj7VeNfjWVfdcp/Bf/PZOf+WEZBdNZ14554HOQawZKrvMD7iqA8X1PpwXA7hM=\n"}}))



 (take 10 words)

 (tap> words))

(def words
  (keys
   (charred/read-json (slurp (io/resource "data.json")))))

(def gwords
  (group-by count words))

(mapv (fn [[k v]]
        [k (count v)])
      gwords)

(def found
  (filter #(not (or (str/ends-with? % "ы")
                    (str/ends-with? % "ь")
                    (str/ends-with? % "ъ")
                    (str/ends-with? % "й")))
          (get gwords 8)))


(defn get-next-start [word used-words]
  (let [next-char (str (last word))]
    (first (filter (fn [cand]
                     (and (str/starts-with? cand next-char)
                          (not (contains? used-words cand))))
                   found))))


(defn get-next-end [word used-words]
  (let [next-char (str (last word))]
    (first (filter (fn [cand]
                     (and (str/ends-with? cand next-char)
                          (not (contains? used-words cand))))
                   found))))

(take 4 found)

(get-next-start "затейщик" #{"затейщик"})

(get-next-end "с" #{})


(defn split-word [word {:keys [x y z]} dir]
  (map-indexed (fn [idx ch]
                 {:name (str ch)
                  :x    (case dir
                          :right (+ x idx)
                          :left (- x idx)
                          x)
                  :y    (case dir
                          :up (- y idx)
                          :down (+ y idx)
                          y)
                  :z    (if (= dir :forward) (+ z idx) z)})
               (if (#{:up :left :forward} dir)
                 (reverse word)
                 word)))

(split-word "затейщик" {:x 0 :y 0 :z 0} :right)
(split-word "скупщица" {:x 7 :y 7 :z 0} :left)
(split-word "конгресс" {:x 7 :y 7 :z 0} :forward)


(def directions
  (cycle [:up :right :down :forward :left :up :right :forward :down :left :up :forward :right :down :left :forward]))


(defn next-word-point [point word dir]
  (let [length (- (count word) 1)]
    (case dir
      :right (update point :x + length)
      :left (update point :x - length)
      :up (update point :y + length)
      :down (update point :y - length)
      :forward (update point :z + length))))


(defn get-words [n]
  (loop [word       "скупщица"
         point      {:x 0 :y 7 :z 0}
         dir-n      0
         result     []
         used-words #{"затейщик" "куконица" "скупщица"}]
    (let [dir        (nth directions dir-n)
          next-word  (get-next-start word used-words)
          next-word' (split-word next-word point dir)]

      (if (= dir-n n)
        result
        (recur next-word
               (next-word-point point next-word dir)
               (inc dir-n)
               (conj result next-word')
               (conj used-words next-word))))))

(def result
  (distinct (flatten (get-words 50))))



(def test-1
  {:towerId "3"
   :letters (concat
             (distinct [{:name "з" :x 0 :y 0 :z 0}
                        {:name "а" :x 1 :y 0 :z 0}
                        {:name "т" :x 2 :y 0 :z 0}
                        {:name "е" :x 3 :y 0 :z 0}
                        {:name "й" :x 4 :y 0 :z 0}
                        {:name "щ" :x 5 :y 0 :z 0}
                        {:name "и" :x 6 :y 0 :z 0}
                        {:name "к" :x 7 :y 0 :z 0}

                        {:name "к" :x 7 :y 0 :z 0}
                        {:name "у" :x 7 :y 1 :z 0}
                        {:name "к" :x 7 :y 2 :z 0}
                        {:name "о" :x 7 :y 3 :z 0}
                        {:name "н" :x 7 :y 4 :z 0}
                        {:name "и" :x 7 :y 5 :z 0}
                        {:name "ц" :x 7 :y 6 :z 0}
                        {:name "а" :x 7 :y 7 :z 0}

                        {:name "с" :x 0 :y 7 :z 0}
                        {:name "к" :x 1 :y 7 :z 0}
                        {:name "у" :x 2 :y 7 :z 0}
                        {:name "п" :x 3 :y 7 :z 0}
                        {:name "щ" :x 4 :y 7 :z 0}
                        {:name "и" :x 5 :y 7 :z 0}
                        {:name "ц" :x 6 :y 7 :z 0}
                        {:name "а" :x 7 :y 7 :z 0}])
             result)})

(comment
 (http/post "https://dtower-api.datsteam.dev/towers"
            {:throw-exceptions false
             :body             (charred/write-json-str test-1)
             :headers          {"Content-Type" "application/json"
                                "token"        "cuKVhArj7VeNfjWVfdcp/Bf/PZOf+WEZBdNZ14554HOQawZKrvMD7iqA8X1PpwXA7hM=\n"}})


 ;; tower result
 (->
  (http/get "https://dtower-api.datsteam.dev/towers/3"
            {:throw-exceptions false
             :headers          {"Content-Type" "application/json"
                                "token"        "cuKVhArj7VeNfjWVfdcp/Bf/PZOf+WEZBdNZ14554HOQawZKrvMD7iqA8X1PpwXA7hM=\n"}})
  :body)


 (def top
   (-> (http/get "https://dtower-api.datsteam.dev/towers/top"
                 {:throw-exceptions false
                  :headers          {"Content-Type" "application/json"
                                     "token"        "cuKVhArj7VeNfjWVfdcp/Bf/PZOf+WEZBdNZ14554HOQawZKrvMD7iqA8X1PpwXA7hM=\n"}})
       :body
       (charred/read-json :key-fn keyword)
       :leaderboard)))
