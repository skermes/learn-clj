(defn multiple? [base x] (== 0 (rem x base)))
(defn x3? [x] (multiple? 3 x))
(defn x5? [x] (multiple? 5 x))

(println
  (reduce + (filter (fn [x] (or (x3? x) (x5? x))) (range 1000))))

