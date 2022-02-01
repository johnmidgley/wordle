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

(defn match [word-entries pred]
  (->> word-entries
       (filter #(pred (:word %)))
       (reduce conj #{})))

(defn match-at-pos* [word-entries char pos]
  (match word-entries #(= char (get % pos))))

(def match-at-pos (memoize match-at-pos*))

(defn match-any-pos* [word-entries char]
  (match word-entries #(str/includes? % (str char))))

(def match-any-pos (memoize match-any-pos*))

(defn match-any-letter [word-entries word]
  (->> (seq word)
       (map #(match-any-pos word-entries %))
       (reduce set/union)))


(defn weight-by-uniform [word-entries]
  (count word-entries))

(defn weight-by-freq [word-entries]
  (reduce + (map :freq word-entries)))

(defn find-split [weight-fn word-entries]
  (let [target-split (/ (weight-fn word-entries) 2)]
    (->> word-entries
         (map (fn [word-entry] 
                {:word-entry word-entry 
                 :match-weight (weight-fn (match-any-letter word-entries (:word word-entry)))}))
         (apply min-key #(abs (- target-split (:match-weight %))))
         (:word-entry))))

(defonce word-freqs (read-freqs (str (System/getProperty "user.dir") "/datasets/unigram-freq.csv") ))
(defonce valid-words (read-words (str (System/getProperty "user.dir") "/datasets/scrabble-twl.txt")))
(defonce valid-words-5 (reduce conj #{} (filter #(= 5 (count %)) valid-words)))
(defonce all-word-entries (read-word-entries (str (System/getProperty "user.dir") "/datasets/unigram-freq.csv")))
(defonce word-entries (reduce conj #{} (filter #(valid-words-5 (:word %)) all-word-entries)))





       
