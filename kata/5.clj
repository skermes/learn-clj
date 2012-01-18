(require '[clojure.java.io :as io])
(import 'java.security.MessageDigest)

(defn all [terms]
  (if (empty? terms)
      true
      (boolean (and (first terms) (all (rest terms))))))

(defn pow [base exponent]
  (if (<= exponent 0)
      1
      (* base (pow base (- exponent 1)))))

;; Casting to short because bytes are signed, and we don't have
;; a ubyte.
(defn byte-to-bits [b]
  (reduce (fn [bits i] (cons (if (odd? (short (/ b (pow 2 i)))) 1 0) bits))
          '()
          (range 8)))

(defn bits-to-num [bits]
  (if (empty? bits)
      0
      (+ (* (first bits) (pow 2 (- (count bits) 1))) (bits-to-num (rest bits)))))

;; Hashes a string using the given strategy and returns a sequence
;; of n-hashes sub-hash sequences, each hash-len bits long.  Hashing
;; strategy must be a valid argument to java.security.MessageDigest.getInstance.
;; If the given strategy returns fewer than (* n-hahes hash-len)
;; bits, returns as many full sub-hashes as possible, and possibly
;; a single truncated sub-hash.
(defn get-hashes [strategy n-hashes hash-len text]
  (let [full-hash (.digest (MessageDigest/getInstance strategy) (.getBytes text))]
    (map bits-to-num (take n-hashes (partition hash-len (flatten (map byte-to-bits full-hash)))))))

(defn vector-of-zeros [len]
  (reduce (fn [coll i] (conj coll 0)) [] (range len)))

(defn bloom-new [strategy n-hashes hash-len]
  (hash-map :hasher (partial get-hashes strategy n-hashes hash-len)
            :filter (vector-of-zeros (pow 2 (max (- hash-len 5) 1))))) ;; Assume 32-bit ints

(defn bloom-add [bloom word]
  (let [hashes ((bloom :hasher) word)]
    (hash-map :hasher (bloom :hasher)
              :filter (reduce (fn [filter hash]
                                (let [replace-idx (long (/ hash 32))
                                      replace-bit (- hash (* replace-idx 32))]
                                  (assoc filter replace-idx (bit-or (nth filter replace-idx) (pow 2 replace-bit)))))
                              (bloom :filter)
                              hashes))))
 
(defn bloom-contains [bloom word]
  (let [hashes ((bloom :hasher) word)
        target-int (fn [hash] (long (/ hash 32)))
        target-bit (fn [hash] (- hash (* (target-int hash) 32)))]
    (all (map (fn [hash] (< 0 (bit-and (nth (bloom :filter) (target-int hash)) (pow 2 (target-bit hash))))) hashes))))

(defn bloom-fill [bloom words]
  (reduce (fn [blm word] (bloom-add blm word)) bloom words))

(defn spellchecker [strategy n-hashes hash-len]
  (with-open [file (io/reader "/usr/share/dict/words")]
    (bloom-fill (bloom-new strategy n-hashes hash-len) (line-seq file))))


(let [checker (spellchecker "MD5" 3 19)
      test-word (fn [word]
                  (do (print word)
                      (print " is")
                      (print (if (bloom-contains checker word) "" " not"))
                      (println " a word.")))]
  (do (test-word "Aaron")
      (test-word "Aarons")
      (test-word "Aaron's")
      (test-word "Aaaron")
      (test-word "Aaroon")))

