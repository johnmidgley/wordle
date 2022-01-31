(ns wordle.core
  (:require [clojure.string :as str]
            [clojure.set :as set]))


(defn read-words [filename]
  (with-open [rdr (clojure.java.io/reader filename)]
    (->> (line-seq rdr)
         (map str/lower-case)
         (filter #(every? (fn [c] (Character/isLetter c)) %))
         (reduce conj #{}))))

(defn word-entry [line]
  (let [[word cnt] (str/split line #",")]
    {:word (str/lower-case word) :count (read-string cnt)}))

(defn read-word-entries [filename]
  (with-open [rdr (clojure.java.io/reader filename)]
    (->> (line-seq rdr)
         (map word-entry)
         (filter #(every? (fn [c] (Character/isLetter c)) (:word %)))
         (reduce conj #{}))))

(defn match [words pred]
  (->> words
       (filter pred)
       (reduce conj #{})))

(defn match-at-pos* [words char pos]
  (match words #(= char (get % pos))))

(def match-at-pos (memoize match-at-pos*))

(defn match-any-pos* [words char]
  (match words #(str/includes? % (str char))))

(def match-any-pos (memoize match-any-pos*))


;; Ingore letters that match all words - not useful for discriminations
(defn match-any-letter [words word]
  (->> (seq word)
       (map #(match-any-pos words %))
       (reduce set/union)))

(defn find-split-word [words]
  (let [target-split (/ (count words) 2)]
   (->> words
        (map (fn [word] 
               {:word word 
                :matches (->> (count (match-any-letter words word))
                              (- target-split)
                              (Math/abs))}))
        (apply min-key :matches))))


(defonce all-words (read-words (str (System/getProperty "user.dir") "/datasets/mac-dictionary.txt")))
; (defonce all-word-entries (read-word-entries (str (System/getProperty "user.dir") "/datasets/unigram-freq.csv")))
(defonce words-5 (match all-words #(= 5 (count %))))




       
