(require '[clojure.string :as string]
         '[clojure.java.io :as io])

;; http://clojuredocs.org/clojure_core/clojure.core/contains_q
(defn valid-word [word] (= word (string/replace word #"[']" "")))

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

(defn statistics [agrams]
  (let [longer (fn [gram1 gram2] (if (> (count (first gram1)) (count (first gram2))) gram1 gram2))
        more (fn [gram1 gram2] (if (> (count gram1) (count gram2)) gram1 gram2))]
    (reduce (fn [result agram] (assoc result :longest (longer (result :longest) agram)
                                             :most (more (result :most) agram)
                                             :count (+ 1 (result :count))))
            (hash-map :longest [""]
                      :most []
                      :count 0)
            agrams)))

(defn print-anagrams [words]
  (let [anagram-groups (anagrams words)
        stats (statistics anagram-groups)]
    (do (doseq [anagram anagram-groups]
          (println (string/join " " anagram)))
        (println "----------------------------------")
        (println "Total anagram sets:  " (stats :count))
        (println "Longest anagram word:" (stats :longest))
        (println "Most anagram words:  " (stats :most)))))

(defn run-on-all-words []
  (with-open [wordlist (io/reader "/usr/share/dict/words")]
    (print-anagrams (set (map string/lower-case (filter valid-word (line-seq wordlist)))))))

(run-on-all-words)

;; TESTING ----------------------------------------------
(require 'clojure.test)

(clojure.test/are [agrams words] (= agrams (set (anagrams words)))
                  #{} []
                  #{} ["a"]
                  #{} ["a" "b" "c"]
                  #{["aab" "aba"]} ["aab" "aba"]
                  #{["aab" "aba"]} ["c" "aab" "d" "aba"]
                  #{["aab" "aba"] ["cde" "ecd" "dce"]} ["aab" "cde" "ecd" "aba" "dce"])


