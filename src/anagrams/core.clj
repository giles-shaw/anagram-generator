(ns anagrams.core
  (:require [clojure.java.io :as io])
  (:require [clojure.string :as string])
  (:require clojure.set)
  (:gen-class))

;;
;preprocessing and io
;;

(defn preprocess [word]
  (-> word
    string/lower-case
    (string/replace #"[^a-z]" "")))

(defn load-dict []
  (map preprocess 
       (string/split
         (slurp (io/resource "corncob_lowercase.txt")) #"\n")))

(defn is-in [char-freqs-one char-freqs-two]
  (every? true? (map (fn [[k v]](>= (get char-freqs-one k 0) v))
                       char-freqs-two)))

(defn subtract-word [char-freqs word]
  (into {} (filter (fn [[_ v]] (> v 0)))
        (reduce #(update %1 %2 dec) char-freqs (char-array word))))

(defn drop-impossible-words
  [phrase-freqs wordlist]
  (filter #(->> % frequencies (is-in phrase-freqs)) wordlist))


;;
;data structure
;;

(defn update-trie [trie word]
  (assoc-in trie word (merge (get-in trie word) {:word word})))

(defn make-trie [words] (reduce update-trie {} words))

(defn query-trie [trie word] (get-in trie word))


;;
;anagrams
;;

(defn contained-words [prefix phrase-chars dictionary-trie]
  (let [remaining-phrase-chars (->> phrase-chars
                                    (filter (fn [[_ ct]] (> ct 0)))
                                    (into {}))
        prefix-compatible-dict (query-trie dictionary-trie prefix)
        options                (apply
                                 clojure.set/intersection
                                 (map (comp set keys)
                                      [remaining-phrase-chars prefix-compatible-dict]))
        current-word           (if (:word prefix-compatible-dict) #{(:word prefix-compatible-dict)} #{})]
    (clojure.set/union
      current-word
      (apply clojure.set/union
             (map #(contained-words (str prefix %)
                                     (update phrase-chars % dec)
                                     dictionary-trie)
                  options)))))


(defn accumulator-successors [[accumulator remainder] dictionary]
  (if (empty? remainder) [[accumulator remainder]]
    (let [words (contained-words "" remainder dictionary)
          remainders (map (partial subtract-word remainder) words)
          accumulators (map #(update accumulator % (fn [v] (if (nil? v) 1 (inc v)))) words)]
      (map vector accumulators remainders))))

(defn anagram-step [accumulator-remainder-pairs dictionary]
  (reduce into #{}
          (map #(accumulator-successors % dictionary)
               (filter (fn [[_ r]] (some? r)) accumulator-remainder-pairs))))

(defn anagram-steps
  [accumulator-remainder-pairs dictionary]
  (if (every? empty? (map second accumulator-remainder-pairs))
    (map first accumulator-remainder-pairs)
    (recur (anagram-step accumulator-remainder-pairs dictionary) dictionary)))

(defn render [phrase-dict]
  (let [repetitions (map #(apply repeat (reverse %)) phrase-dict)]
    (->> repetitions flatten sort (interpose " ") (apply str))))

(defn anagrams [phrase dictionary]
  (let [char-freqs (-> phrase preprocess frequencies)]
    (sort (map render (anagram-steps #{[{} char-freqs]} dictionary)))))

(defn main
  [phrase]
  (let [dictionary (make-trie (drop-impossible-words
                                (frequencies (preprocess phrase)) (load-dict)))
        anagrams (anagrams phrase dictionary)]
   (apply str (interpose "\n" anagrams))))


;;
;test examples
;;
; (def test-phrase "rearrange me")
; (def filtered-dict (make-trie (drop-impossible-words (frequencies test-phrase) (load-dict))))
; (count (anagrams test-phrase filtered-dict))

; (time (main test-phrase))
; (println (main test-phrase))
