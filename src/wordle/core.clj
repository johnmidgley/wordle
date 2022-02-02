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


(defn mode[freqs words]
  (if (empty? words)
    nil
    (apply max-key #(get freqs % 1) words)))

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


(defn apply-segment [words [op char pos]]
  (let [op-fn (case op 
                \+ set/intersection
                \- set/difference)
        matches (match-char words char pos)]    
    (op-fn words matches)))


(defn parse-path [path]
  (map (fn [[op char pos]] [op char (if pos (read-string (str pos)))])
       (re-seq #"[+-]\w\d?" path)))


(defn apply-path [words path]
  (let [segments (parse-path path)]
    (reduce apply-segment words segments)))

; Still not perfect - could be more constraining by taking into account previously matched letters
; e.g. state as a guess - the second t could produce -t2-t3-t4, since there's only one t, but 
; currenlty it just produces +t-t3
(defn check-guess [target word]
  (let [letters (apply hash-set target)]
   (->> (map vector target word)
        (map-indexed vector)
        (map (fn [[pos [tl wl]]]
               (str (if (letters wl) \+ \-)
                    wl
                    (if (= tl wl) pos (str "-" wl pos)))))
        (apply str))))

(defn guess-by-mode [freqs words path]
  (mode freqs words))

(defn play
  ([words guess-fn target]
   (reverse (distinct (play words guess-fn target "" 0))))
  ([words guess-fn target path depth]
   (cond
     (> depth 10) [nil]
     (<= (count words) 1) [(words target)]
     :default (let [guess (guess-fn words path)
                    new-path (str path (check-guess target guess))
                    new-words (apply-path words new-path)]
                (conj (play new-words guess-fn target new-path (inc depth)) guess)))))

(defonce freqs (read-freqs (str (System/getProperty "user.dir") "/datasets/unigram-freq.csv")))
(defonce all-words (read-words (str (System/getProperty "user.dir") "/datasets/scrabble-twl.txt")))
(defonce words (reduce conj #{} (filter #(= 5 (count %)) all-words)))

(defn play-by-mode [target]
  (play words (partial guess-by-mode freqs) target))
       
