(require '[clojure.string :as str]
         '[clojure.java.io :as io])

;; READABLE VERSION --------------------------------------------------

(let [n-letter-words (fn [n dictionary] (filter #(= n (count %)) dictionary))]
  (def six-letter-words   (partial n-letter-words 6))
  (def five-letter-words  (partial n-letter-words 5))
  (def four-letter-words  (partial n-letter-words 4))
  (def three-letter-words (partial n-letter-words 3))
  (def two-letter-words   (partial n-letter-words 2))
  (def one-letter-words   (partial n-letter-words 1)))

(defn prefix-of? [word prefix]
  (= prefix (subs word 0 (count prefix))))

(defn cartesian-product [xs ys]
  (for [x xs
        y ys]
    [x y]))

(defn suffix [word prefix]
  (subs word (count prefix)))

(defn add-compound [word pref suffixes compounds]
  (let [suff (suffix word pref)]
    (if (and (prefix-of? word pref) (suffixes suff))
        (conj compounds {:pref pref :suff suff :word word})
        compounds)))

(defn compound-words [prefixes suffixes dictionary]
  (let [suffixes (set suffixes)]
    (reduce #(add-compound (first %2) (second %2) suffixes %1)
            []
            (cartesian-product dictionary prefixes))))

(defn six-letter-compounds [dictionary]
  (let [ones   (one-letter-words dictionary)
        twos   (two-letter-words dictionary)
        threes (three-letter-words dictionary)
        fours  (four-letter-words dictionary)
        fives  (five-letter-words dictionary)
        sixes  (six-letter-words dictionary)]
    (flatten [(compound-words ones fives sixes)
              (compound-words twos fours sixes)
              (compound-words threes threes sixes)
              (compound-words fours twos sixes)
              (compound-words fives ones sixes)])))

(defn pretty-print [compounds]
  (doseq [compound compounds]
    (println (compound :pref) "+" (compound :suff) "=" (compound :word))))

(defn run-on-all-words []
  (with-open [wordlist (io/reader "/usr/share/dict/words")]
    (pretty-print (six-letter-compounds (line-seq wordlist)))))

;; Last time, this took ~5:15:00
;; (run-on-all-words)

;; TESTING -----------------------------------------------------

(require '[clojure.test :as test])

(test/are [x y] (= x (six-letter-compounds y))
  [] []
  [] ["foodie"]
  [] ["a" "bc" "def" "ghij" "klmno" "qrstuv"]
  [] ["a" "ab" "abc" "abcd" "abcde" "abcdef"]
  [] ["f" "ef" "def" "cdef" "bcdef" "abcdef"])

(test/are [pre suf word dict] (= [{:pref pre :suff suf :word word}]
                                 (six-letter-compounds dict))
  "a" "bcdef" "abcdef" ["bcdef" "a" "abcdef"]
  "ab" "cdef" "abcdef" ["abcdef" "ab" "cdef"]
  "abc" "def" "abcdef" ["abc" "def" "abcdef"]
  "abcd" "ef" "abcdef" ["abcd" "abcdef" "ef"]
  "abcde" "f" "abcdef" ["f" "abcdef" "abcde"])



