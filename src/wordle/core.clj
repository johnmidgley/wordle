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

(defn word-entry [line]
  (let [[word freq] (parse-line line)]
    {:word word :freq freq}))

;; Assumes csv with word,freq on each line
(defn read-word-entries [filename]
  (with-open [rdr (clojure.java.io/reader filename)]
    (->> (line-seq rdr)
         (map word-entry)
         (filter #(every? (fn [c] (Character/isLetter c)) (:word %)))
         ( conj #{}))))

(defn match [words pred]
  (->> words
       (filter #(pred %))
       (reduce conj #{})))

(defn match-at-pos* [words char pos]
  (match words #(= char (get % pos))))

(def match-at-pos (memoize match-at-pos*))

(defn match-any-pos* [words char]
  (match words #(str/includes? % (str char))))

(def match-any-pos (memoize match-any-pos*))

(defn match-any-letter [words word]
  (->> (seq word)
       (map #(match-any-pos words %))
       (reduce set/union)))


(defn weight-by-uniform [words]
  (count words))

(defn weight-by-freq [freqs words]
  (reduce + (map #(get freqs % 1) words)))

(defn find-split [weight-by words]
  (let [target-split (/ (weight-by words) 2)]
    (->> words
         (map (fn [word] 
                {:word word 
                 :weight (weight-by (match-any-letter words word))}))
         (apply min-key #(abs (- target-split (:weight %))))
         (:word))))

(defn parse-path [path]
  (re-seq #"[+-]\w\d?" path))

(defonce freqs (read-freqs (str (System/getProperty "user.dir") "/datasets/unigram-freq.csv") ))
(defonce all-words (read-words (str (System/getProperty "user.dir") "/datasets/scrabble-twl.txt")))
(defonce words (reduce conj #{} (filter #(= 5 (count %)) all-words)))






       
