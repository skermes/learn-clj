(require 'clojure.string)

(defn get-table-rows []
  (let [data (slurp "kata/weather.dat")]
    (clojure.string/split-lines (subs data 162 (- (count data) 8)))))

(defn calmest-day []
  (let [day #(clojure.string/trim (subs % 2 4))
        max #(Integer/parseInt (subs % 6 8))
        min #(Integer/parseInt (subs % 12 14))
        rows (take 30 (drop 2 (get-table-rows)))]
    (day (reduce (fn [row1 row2]
                   (let [spread1 (- (max row1) (min row1))
                         spread2 (- (max row2) (min row2))]
                     (if (< spread1 spread2) row1 row2)))
                 rows))))

(print "Calmest day is: ")
(println (calmest-day))

