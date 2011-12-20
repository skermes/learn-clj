(defn fibs-helper [a b n]
    (if (== 0 n)
	[]
	(cons a (fibs-helper b (+ a b) (- n 1)))))

(defn fibs [n]
    (fibs-helper 1 2 n))

(defn even-fibs [n] (filter even? (fibs (* 3 n))))

(defn until [n lst] (take-while (fn [x] (< x n)) lst))

(println
    (reduce + (until (* 4 1000 1000) (even-fibs 20))))

