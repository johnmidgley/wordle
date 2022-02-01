(ns wordle.core
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn abs [n]
  (if (neg? n) (- n) n))


;; Assumes one word per line
(defn read-words [filename]
  (with-open [rdr (clojure.java.io/reader filename)]
    (->> (line-seq rdr)
         (filter #(every? (fn [c] (Character/isLetter c)) %))
         (map str/lower-case)
         (reduce conj #{}))))

(defn parse-line [line]
  (let [[word freq] (str/split line #",")]
    [(str/lower-case word) (read-string freq)]))

(defn read-freqs [filename]
  (with-open [rdr (clojure.java.io/reader filename)]
    (->> (line-seq rdr) 
         (mapcat  parse-line) 
         (apply hash-map))))

(defn match-pred [words pred]
  (->> words
       (filter #(pred %))
       (reduce conj #{})))

(defn match-char* 
  ([words char]
   (match-char* words char nil))
  ([words char pos]
   (let [pred (if pos
                #(= char (get % pos)) 
                #(str/includes? % (str char)))]
     (match-pred words pred))))

(def match-char (memoize match-char*))


(defn match-any-letter [words letters]
  (->> (seq letters)
       (map #(match-char words %))
       (reduce set/union)))

(defn weight-by-uniform [words]
  (count words))


(defn weight-by-freq [freqs words]
  (reduce + (map #(get freqs % 1) words)))


(defn find-split [weight-by words]
  (let [target-split (/ (weight-by words) 2)]
    (->> words
         (pmap (fn [word] 
                {:word word 
                 :weight (weight-by (match-any-letter words word))}))
         (apply min-key #(abs (- target-split (:weight %))))
         (:word))))


(defn op-fn [op]
  (case op 
    \+ set/intersection
    \- set/difference))


(defn apply-segment [words [op char pos]]
  (let [f (op-fn op)
        matches (match-char words char pos)]    
    (f words matches)))


(defn parse-path [path]
  (map (fn [[op char pos]] [op char (if pos (read-string (str pos)))])
       (re-seq #"[+-]\w\d?" path)))


(defn apply-path [words path]
  (let [segments (parse-path path)]
    (reduce apply-segment words segments)))



(defonce freqs (read-freqs (str (System/getProperty "user.dir") "/datasets/unigram-freq.csv")))
(defonce all-words (read-words (str (System/getProperty "user.dir") "/datasets/scrabble-twl.txt")))
(defonce words (reduce conj #{} (filter #(= 5 (count %)) all-words)))







       
