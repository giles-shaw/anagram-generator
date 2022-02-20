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

(defn filter-wordlist
  [allowed-chars wordlist]
  (filter #(is-in allowed-chars (frequencies %)) wordlist))


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

(defn contained-words [prefix phrase-chars dictionary]
  (let [phrase-chars    (->> phrase-chars
                             (filter (fn [[_ ct]] (> ct 0)))
                             (into {}))
        prefixed-dict   (query-trie dictionary prefix)
        options         (apply clojure.set/intersection
                               (map (comp set keys) [phrase-chars prefixed-dict]))
        successor-words (apply clojure.set/union
                               (map #(contained-words (str prefix %)
                                                      (update phrase-chars % dec)
                                                      dictionary)
                                    options))]
    (if (:word prefixed-dict) (conj successor-words prefix) successor-words)))


(defn next-word-transfers [[accumulator remainder] dictionary]
  (if (empty? remainder) [[accumulator remainder]]
    (let [words        (contained-words "" remainder dictionary)
          remainders   (map (partial subtract-word remainder) words)
          accumulators (map (fn [w] (update accumulator w #(inc (or % 0)))) words)]
      (map vector accumulators remainders))))

(defn update-accumulators 
  "Returns the set of all (accumulator, remainder) pairs that can result from
  transferring a word from each remainder in `accumulator-remainder-pairs` to the
  corresponding accumulator. Pairs in `accumulator-remainder-pairs` whose remainder is
  nonempty but does not contain any words in `dictionary` are removed, and pairs whose
  remainder is empty are carried forward without change."
  [accumulator-remainder-pairs dictionary]
  (let [updates (map #(next-word-transfers % dictionary)
                     accumulator-remainder-pairs)] 
  (reduce into #{} updates)))  ; remove repetitions

(defn anagram-steps
  [pairs dictionary]
  (let [accumulations (map first pairs)
        remainders    (map second pairs)]
  (if (every? empty? remainders) accumulations
    (recur (update-accumulators pairs dictionary) dictionary))))

; (defn anagram-steps [pairs dictionary]
;   (let [steps (iterate #(update-accumulators % dictionary) pairs)
;         some-non-empty-remainder? (fn [acc-remainder-pairs]
;                                     (some? (map (comp seq :remainder)
;                                                 acc-remainder-pairs)))]
;     (map :accumulator
;          (first (drop-while some-non-empty-remainder? steps)))))

(defn render [word-dict]
  (let [repetitions (map #(apply repeat (reverse %)) word-dict)]
    (->> repetitions flatten sort (interpose " ") (apply str))))

(defn anagrams [phrase-chars dictionary]
  (sort (map render (anagram-steps #{[{} phrase-chars]} dictionary))))

(defn main
  [phrase]
  (let [phrase-chars (-> phrase preprocess frequencies)
        dictionary   (->> (load-dict) (filter-wordlist phrase-chars) make-trie)
        anagrams (anagrams phrase-chars dictionary)]
   (apply str (interpose "\n" anagrams))))

;;
;test examples
;;
; (def test-phrase "parmesan")
; (def phrase-chars (frequencies (preprocess test-phrase)))
; (def filtered-dict (make-trie (filter-wordlist phrase-chars (load-dict))))
; (count (anagrams phrase-chars filtered-dict))
; (first (anagrams phrase-chars filtered-dict))

; (time (main test-phrase))
; (println (main test-phrase))
