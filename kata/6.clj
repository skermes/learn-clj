(require '[clojure.string :as string]
         '[clojure.java.io :as io])

(defn anagram-map [words]
  (let [key (fn [word] (string/join "" (sort word)))]
    (reduce (fn [anagrams word] 
              (assoc anagrams 
                     (key word) 
                     (conj (get anagrams (key word) []) word)))
            (hash-map)
            words)))

(defn anagrams [words]
  (filter #(> (count %) 1) (vals (anagram-map words))))

(defn print-anagrams [words]
  (let [anagram-groups (anagrams words)]
    (do (println "Total anagram tuples:" (count anagram-groups))
        (doseq [anagram anagram-groups]
          (println (string/join " " anagram)))
        (println "Total anagram tuples:" (count anagram-groups)))))

(defn run-on-all-words []
  (with-open [wordlist (io/reader "/usr/share/dict/words")]
    (print-anagrams (line-seq wordlist))))

;; (run-on-all-words)

;; TESTING ----------------------------------------------
(require 'clojure.test)

(clojure.test/are [agrams words] (= agrams (set (anagrams words)))
                  #{} []
                  #{} ["a"]
                  #{} ["a" "b" "c"]
                  #{["aab" "aba"]} ["aab" "aba"]
                  #{["aab" "aba"]} ["c" "aab" "d" "aba"]
                  #{["aab" "aba"] ["cde" "ecd" "dce"]} ["aab" "cde" "ecd" "aba" "dce"])


