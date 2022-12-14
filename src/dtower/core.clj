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


(def WORD_LEN 8)


(def found
  (filter #(not (or (str/ends-with? % "ы")
                    (str/ends-with? % "ь")
                    (str/ends-with? % "ъ")
                    (str/ends-with? % "й")))
          (get gwords WORD_LEN)))


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
  #_(cycle [:forward :up :right :down :forward :left :up :right :forward :down :left :up :forward :right :down :left])
  (cycle [:forward :right :forward :left]))


(defn next-word-point [point word dir]
  (let [length (- (count word) 1)]
    (case dir
      :right (update point :x + length)
      :left (update point :x - length)
      :up (update point :y - length)
      :down (update point :y + length)
      :forward (update point :z + length))))



(defn get-word-up [word used-words]
  (let [letter (str (first word))]
    (first (filter (fn [cand]
                     (and (str/ends-with? cand letter)
                          (not (contains? used-words cand))))
                   found))))

(defn get-word-down [word used-words]
  (let [letter (str (last word))]
    (first (filter (fn [cand]
                     (and (str/starts-with? cand letter)
                          (not (contains? used-words cand))))
                   found))))

(defn get-word-left [word used-words]
  (let [letter (str (last word))]
    (first (filter (fn [cand]
                     (and (str/ends-with? cand letter)
                          (not (contains? used-words cand))))
                   found))))

(defn get-word-right [word used-words]
  (let [letter (str (first word))]
    (first (filter (fn [cand]
                     (and (str/starts-with? cand letter)
                          (not (contains? used-words cand))))
                   found))))

(defn get-word-forward [word used-words]
  (let [letter (str (first word))]
    (first (filter (fn [cand]
                     (and (str/ends-with? cand letter)
                          (not (contains? used-words cand))))
                   found))))

(defn get-next-word [word used-words dir prev-dir]
  (if (= :forward prev-dir)
    (case dir
      :up (get-word-up word used-words)
      :down (get-word-right word used-words)
      :left (get-word-up word used-words)
      :right (get-word-right word used-words)
      :forward (case prev-dir
                 :up (get-word-forward word used-words)
                 :down (get-word-left word used-words)
                 :left (get-word-forward word used-words)
                 :right (get-word-left word used-words)
                 :forward (get-word-forward word used-words)
                 nil (get-word-forward word used-words)))

    (case dir
      :up (get-word-up word used-words)
      :down (get-word-down word used-words)
      :left (get-word-left word used-words)
      :right (get-word-right word used-words)
      :forward (case prev-dir
                 :up (get-word-forward word used-words)
                 :down (get-word-left word used-words)
                 :left (get-word-forward word used-words)
                 :right (get-word-left word used-words)
                 :forward (get-word-forward word used-words)
                 nil (get-word-forward word used-words)))))

;; (-> "затейщик"
;;     (get-next-word #{} :down)
;;     (get-next-word #{} :left)
;;     (get-next-word #{} :forward)
;;     )


(defn get-words [n]
  (loop [word       "скупщица"
         point      {:x 0 :y 7 :z 0}
         dir-n      0
         result     []
         used-words #{"затейщик" "куконица" "скупщица"}]
    (let [dir        (nth directions dir-n)
          prev-dir   (if (= 0 dir-n)
                       nil
                       (nth directions (dec dir-n)))
          next-word  (get-next-word word used-words dir prev-dir)
          next-word' (split-word next-word point dir)]

      (when-not next-word
        (throw (Exception. (str "Can not find a word to continue. Last word used: " word ". Direction: " dir ". Iteration: " dir-n))))

      (if (= dir-n n)
        {:letters result :used-words used-words}
        (recur next-word
               (next-word-point point next-word dir)
               (inc dir-n)
               (conj result next-word')
               (conj used-words next-word))))))


(def calculation (get-words 25))

(def result
  (distinct (flatten (:letters calculation))))


(def letters
  (distinct
   (concat
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
    result))
  )


(take 100 letters)

;; (add-supports letters 0 (:used-words calculation) (int (/ WORD_LEN 2)))

(defn add-supports [letters level-1 used-words support-position]
  (let [level-2 (+ level-1 WORD_LEN -1)

        level-1-letters (filter #(and (or (= support-position (:x %))
                                          (= support-position (:y %)))
                                      (= level-1 (:z %)))
                                letters)

        level-2-letters (filter #(and (or (= support-position (:x %))
                                          (= support-position (:y %)))
                                      (= level-2 (:z %)))
                                letters)

        letters-to-match (group-by (juxt :x :y)
                                   (concat level-1-letters level-2-letters))


        letter-candidates (filter (fn [[k v]]
                                    (= 2 (count v)))
                                  letters-to-match)]

    (map (fn [[k [l1 l2]]]
           [(conj k level-1)
            (first
             (filter (fn [w]
                       (and (< 7 (count w))
                            (> 13 (count w))
                            (not= WORD_LEN (count w))
                            (= (nth (:name l1) 0) (nth w (dec (count w))))
                            (= (nth (:name l2) 0) (nth w (- (count w) WORD_LEN)))))
                     words))])
         letter-candidates)))


(mod WORD_LEN 48)


(def offsets (cycle [ 2 3 4 5]))

(range 0 50 (dec WORD_LEN))

(defn strengthen [letters]
  (let [max-z (apply max (map :z letters))
        steps (range 0 max-z (dec WORD_LEN))
        used-words (:used-words calculation)]
    (reduce
        (fn [{:keys [used-words col-letters]} step]
          (let [interval-1 (rand-nth (range 1 (dec (dec WORD_LEN))))
                ;; (int (/ WORD_LEN (nth offsets step)))
                interval-2  (inc interval-1)

                columns-1 (add-supports letters step used-words interval-1)

                new-used-words-1 (set (map last columns-1))
                used-words' (clojure.set/union used-words new-used-words-1)

                columns-2 (add-supports letters step used-words' interval-2)

                new-used-words-2 (set (map last columns-2))
                used-words'' (clojure.set/union used-words' new-used-words-2)

                letters-1 (apply concat
                                 (map (fn [[[x y z] word]]
                                        (when word
                                          (split-word word {:x x :y y :z z} :forward)))
                                      columns-1))


                letters-2 (apply concat
                                 (map (fn [[[x y z] word]]
                                        (when word
                                          (split-word word {:x x :y y :z z} :forward)))
                                      columns-2))

                new-letters (concat letters-1 letters-2)

                ]
            {:used-words used-words'
             :col-letters (concat col-letters letters-1)}
            ))
        {:used-words used-words
         :col-letters []}
        steps
     )
    
    )
  )


(:letters calculation)

(def strengthned-letters
  #_(loop [more-letters (strengthen letters)]
    (let [ltrs (distinct (concat letters
                                 (:col-letters more-letters)))]
      (if (empty? (filter #(< 1 (count (second %)))
                          (group-by (juxt :x :y :z) ltrs)))
        ltrs
        (recur (strengthen letters))))))


(filter #(and (= 7 (:x %))
              (= 5 (:y %)))
        strengthned-letters)


(def test-1
  {:towerId "23"
   :letters strengthned-letters})


(defn get-tower-status [tower-id]
  (->
   (http/get (str "https://dtower-api.datsteam.dev/towers/" tower-id)
             {:throw-exceptions false
              :headers          {"Content-Type" "application/json"
                                 "token"        "cuKVhArj7VeNfjWVfdcp/Bf/PZOf+WEZBdNZ14554HOQawZKrvMD7iqA8X1PpwXA7hM=\n"}})
   :body))


(defn top []
  (-> (http/get "https://dtower-api.datsteam.dev/towers/top"
                {:throw-exceptions false
                 :headers          {"Content-Type" "application/json"
                                    "token"        "cuKVhArj7VeNfjWVfdcp/Bf/PZOf+WEZBdNZ14554HOQawZKrvMD7iqA8X1PpwXA7hM=\n"}})
      :body
      (charred/read-json :key-fn keyword)
      :leaderboard))


(get-tower-status (:towerId test-1))

(top)


(comment
 (http/post "https://dtower-api.datsteam.dev/towers"
            {:throw-exceptions false
             :body             (charred/write-json-str test-1)
             :headers          {"Content-Type" "application/json"
                                "token"        "cuKVhArj7VeNfjWVfdcp/Bf/PZOf+WEZBdNZ14554HOQawZKrvMD7iqA8X1PpwXA7hM=\n"}})


 ;; tower result
 


 )
