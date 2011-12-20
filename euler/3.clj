(ns skermes.euler
  (:require [clojure.set :as set]))

(defn multiple? [base x] (== 0 (rem x base)))

(defn erastothenes [lst]
    (if (empty? lst)
        []
        (let [p (first lst)]
            (cons p (erastothenes (remove (fn [x] (multiple? p x)) (rest lst)))))))

(defn primes-up-to [x] (set (erastothenes (range 2 x))))

(defn factors [n]
    (let [small-factors (filter (fn [x] (multiple? x n)) (range 1 (+ 1 (Math/ceil (Math/sqrt n)))))]
        (set (concat small-factors (map (fn [x] (/ n x)) small-factors)))))

(defn prime? [x]
    (== 2 (count (factors x)))) 

;; (println (factors 45))
;; (println (primes-up-to 50))
(println (filter prime? (factors 600851475143)))

