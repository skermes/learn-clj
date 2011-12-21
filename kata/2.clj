(defn search-rec [needle haystack]
  (letfn [(search [first last]
            (if (> first last)
                -1
                (let [mid (+ first (Math/floor (/ (- last first) 2)))]
                  (cond (== needle (nth haystack mid)) mid
                        (> needle (nth haystack mid)) (search (+ mid 1) last)
                        :else (search first (- mid 1))))))]
    (int (search 0 (- (count haystack) 1)))))

(defn test-search [searcher]
  (do
    (print "Should be 5: ")
    (println (searcher 5 (range 10)))
    (print "Should be 0: ")
    (println (searcher 0 (range 10)))
    (print "Should be 9: ")
    (println (searcher 9 (range 10)))
    (print "Should be -1: ")
    (println (searcher 5 (range 0 10 -2)))
    (print "Should be -1: ")
    (println (searcher 1 (range 2 10)))
    (print "Should be -1: ")
    (println (searcher 12 (range 10)))
    (print "Should be 2 ")
    (println (searcher 5 [1 3 5 7 9]))
    (print "Should be 0 ")
    (println (searcher 1 [1 3 5 7 9]))
    (print "Should be 4 ")
    (println (searcher 9 [1 3 5 7 9]))
))

(test-search search-rec)

