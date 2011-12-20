;; Euler 4.  Largest palindromic number from set of products of 3-digit numbers.
;; smallest candidate: 100 * 100 = 10,000
;; largest candidate: 999 * 999 = 998,001
;; 

;; This is kind of a hack, but it's the best I've got.
(defn char-list [s]
  (map (fn [x] x) (.toCharArray s)))

(defn palindrome? [n]
  (let [as-str (char-list (str n))]
    (.equals as-str (reverse as-str))))

;; (println (palindrome? 1001))
;; (println (palindrome? 12344321))
;; (println (palindrome? 1234))
;; (println (palindrome? 123432))

(defn has-3-digit-factors [x]
  

(println
  (some (fn [x] (if (palindrome? x) x false)) (range 998001 10000 -1)))

