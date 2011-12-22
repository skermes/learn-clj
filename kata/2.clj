(defn search-rec [needle haystack]
  (letfn [(search [first last]
            (if (> first last)
                -1
                (let [mid (+ first (Math/floor (/ (- last first) 2)))]
                  (cond (== needle (nth haystack mid)) mid
                        (> needle (nth haystack mid)) (search (+ mid 1) last)
                        :else (search first (- mid 1))))))]
    (int (search 0 (- (count haystack) 1)))))

(defn search-loop [needle haystack]
  (letfn [(get-mid [bot top] (+ bot (Math/floor (/ (- top bot) 2))))]
    (loop [first 0
           last (- (count haystack) 1)]
      (when (<= last first)
        (let [mid (get-mid first last)]
          (cond (== needle (nth haystack mid)) mid
                (> needle (nth haystack mid)) (recur (+ mid 1) last)
                :else (recur first (- mid 1))))))))

(defn search-slice [needle haystack]
  (letfn [(search [offset haystack]
            (if (empty? haystack)
                -1
               (let [mid (int (Math/floor (/ (count haystack) 2)))]
                 (cond (== needle (nth haystack mid)) (+ mid offset)
                       (> needle (nth haystack mid)) (search (+ offset mid 1) (drop (+ mid 1) haystack))
                       :else (search offset (take mid haystack))))))]
    (search 0 haystack)))

(defn test-search [name searcher]
  (do
    (print name)
    (println ":")
    (print "Should be  5:  ")
    (println (searcher 5 (range 10)))
    (print "Should be  0:  ")
    (println (searcher 0 (range 10)))
    (print "Should be  9:  ")
    (println (searcher 9 (range 10)))
    (print "Should be -1: ")
    (println (searcher 5 (range 0 10 2)))
    (print "Should be -1: ")
    (println (searcher 1 (range 2 10)))
    (print "Should be -1: ")
    (println (searcher 12 (range 10)))
    (print "Should be  2:  ")
    (println (searcher 5 [1 3 5 7 9]))
    (print "Should be  0:  ")
    (println (searcher 1 [1 3 5 7 9]))
    (print "Should be  4:  ")
    (println (searcher 9 [1 3 5 7 9]))
    (print "Should be -1: ")
    (println (searcher 1 []))
    (println)
))

(test-search "Recursive search" search-rec)
;; (test-search "Loop search" search-loop)
(test-search "Recursive slice search" search-slice)



