(require '[clojure.string :as string]
         '[clojure.java.io :as io])

(defn decorate [words]
  (map (fn [word] (hash-map :key (string/join "" (sort word)) :val word)) words))

(defn anagram-map [words]
  (reduce (fn [anagrams decorated] (assoc anagrams (decorated :key) (conj (get anagrams (decorated :key) []) (decorated :val))))
          (hash-map)
          (decorate words)))

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

(run-on-all-words)

