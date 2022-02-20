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

(defn constructible? [char-freqs word]
    (->> word
         frequencies
         (map (fn [[ch ct]] (>= (get char-freqs ch 0) ct)))
         (every? true?))
  ; (every? true? (map (fn [[k v]](>= (get char-freqs k 0) v)) (frequencies word)))
  )

(defn subtract-word [char-freqs word]
  (into {} (filter (fn [[_ v]] (> v 0)))
        (reduce #(update %1 %2 dec) char-freqs (char-array word))))

(defn filter-wordlist
  [allowed-chars wordlist]
  (filter (partial constructible? allowed-chars) wordlist))


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

(defn contained-words 
  "Returns the set of words in `dictionary` beginning with `prefix` which can
  be constructed from `phrase-chars`."
  [prefix phrase-chars dictionary]
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

(defn next-word-transfers
  "Returns a sequence of all `{:keys [accumulator remainder]}` maps that result from
  transferring a valid word from `remainder` to `accumulator`."
  [{:keys [accumulator remainder] :as pair} dictionary]
  (if (empty? remainder) [pair]
    (let [words        (contained-words "" remainder dictionary)
          remainders   (map (partial subtract-word remainder) words)
          accumulators (map (fn [w] (update accumulator w #(inc (or % 0)))) words)]
      (map #(zipmap [:accumulator :remainder] [%1 %2]) accumulators remainders))))

(defn update-accumulators 
  "Returns the set of all `{:keys [accumulator remainder]}` maps that can
  result from transferring a word from a `remainder` value in
  `accumulator-remainder-pairs` to the corresponding `accumulator` value. Maps
  in `accumulator-remainder-pairs` whose `remainder` is nonempty but does not
  contain any words in `dictionary` are removed. Pairs whose `remainder` is
  empty are carried forward unchanged."
  [accumulator-remainder-pairs dictionary]
  (let [updates (map #(next-word-transfers % dictionary)
                     accumulator-remainder-pairs)] 
  (reduce into #{} updates)))  ; deduplicate

(defn anagrams
  "Returns the sequence of all frequency maps whose keys are words from
  `dictionary` which can be be constructed using characters from `phrase-chars`
  without replacement,"
  [phrase-chars dictionary]
  (let [init-pairs #{{:accumulator {} :remainder phrase-chars}}
        steps      (iterate #(update-accumulators % dictionary) init-pairs)
        remainder? (fn [acc-remainder-pairs]
                     (some seq (map :remainder acc-remainder-pairs)))]
    (map :accumulator (first (drop-while remainder? steps)))))


;;
; display and main
;;

(defn render-anagram [word-dict]
  (let [repetitions (map #(apply repeat (reverse %)) word-dict)]
    (->> repetitions flatten sort (interpose " ") (apply str))))

(defn render [anagram-list]
  (->> anagram-list (map render-anagram) sort (interpose "\n") (apply str)))

(defn main
  [phrase]
  (let [phrase-chars (-> phrase preprocess frequencies)
        dictionary   (->> (load-dict) (filter-wordlist phrase-chars) make-trie)
        results      (anagrams phrase-chars dictionary)]
   (render results)))


;;
;test examples
;;
(def test-phrase "parmesan")
; (def phrase-chars (frequencies (preprocess test-phrase)))
; (def filtered-dict (make-trie (filter-wordlist phrase-chars (load-dict))))
; (count (anagrams phrase-chars filtered-dict))
; (first (anagrams phrase-chars filtered-dict))

; (time (main test-phrase))
(println (main test-phrase))
