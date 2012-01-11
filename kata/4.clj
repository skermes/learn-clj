(require '[clojure.string :as string])

(defn get-table-rows [file prelude-size header-rows data-rows]
  (let [data (slurp file)]
    (take data-rows 
      (drop header-rows
        (filter #(not (string/blank? (string/trim (string/replace % "-" ""))))
                (string/split-lines (subs data prelude-size (- (count data) 8))))))))

(defn key-with-smallest-spread [key-cols front-cols back-cols rows]
  (let [key #(string/trim (subs % (first key-cols) (second key-cols)))
        front #(Integer/parseInt (subs % (first front-cols) (second front-cols)))
        back #(Integer/parseInt (subs % (first back-cols) (second back-cols)))]
    (first (reduce (fn [row1 row2] (if (< (second row1) (second row2)) row1 row2))
                   (map (fn [r] [(key r) (Math/abs (- (front r) (back r)))]) rows)))))

(defn calmest-day []
  (key-with-smallest-spread [2 4]
                            [6 8]
                            [12 14]
                            (get-table-rows "kata/weather.dat" 162 2 30)))

(defn evenest-team []
  (key-with-smallest-spread [7 20]
                            [43 45]
                            [50 52]
                            (get-table-rows "kata/football.dat" 171 1 21)))

(print "Calmest day is: ")
(println (calmest-day))

(print "Evenent-scoring team is: ")
(println (evenest-team))

