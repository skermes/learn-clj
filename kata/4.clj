(require 'clojure.string)

(defn get-table-rows []
  (let [data (slurp "kata/weather.dat")]
    (clojure.string/split-lines (subs data 162 (- (count data) 8)))))

(defn calmest-day []
  (let [day #(clojure.string/trim (subs % 2 4))
        max #(Integer/parseInt (subs % 6 8))
        min #(Integer/parseInt (subs % 12 14))
        rows (take 30 (drop 2 (get-table-rows)))]
    (first (reduce (fn [row1 row2] (if (< (second row1) (second row2)) row1 row2)) 
                   (map (fn [r] [(day r) (- (max r) (min r))]) rows)))))

(print "Calmest day is: ")
(println (calmest-day))

