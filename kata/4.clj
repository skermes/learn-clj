(require '[clojure.string :as string])

(defn get-table-rows [file header-size]
  (let [data (slurp file)]
    (filter #(not (string/blank? (string/trim (string/replace % "-" ""))))
            (string/split-lines (subs data header-size (- (count data) 8))))))

(defn calmest-day []
  (let [day #(string/trim (subs % 2 4))
        max #(Integer/parseInt (subs % 6 8))
        min #(Integer/parseInt (subs % 12 14))
        rows (take 30 (drop 2 (get-table-rows "kata/weather.dat" 162)))]
    (first (reduce (fn [row1 row2] (if (< (second row1) (second row2)) row1 row2)) 
                   (map (fn [r] [(day r) (- (max r) (min r))]) rows)))))

(defn evenest-team []
  (let [name #(string/trim (subs % 7 20))
        for #(Integer/parseInt (subs % 43 45))
        against #(Integer/parseInt (subs % 50 52))
        rows (take 21 (drop 1 (get-table-rows "kata/football.dat" 171)))]
    (first (reduce (fn [row1 row2] (if (< (second row1) (second row2)) row1 row2))
                   (map (fn [r] [(name r) (Math/abs (- (for r) (against r)))]) rows)))))

(print "Calmest day is: ")
(println (calmest-day))

(print "Evenent-scoring team is: ")
(println (evenest-team))

