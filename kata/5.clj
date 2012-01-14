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

(defn bloom-new [] #{})

(defn bloom-add [bloom hashes]
  (reduce (fn [blm hash] (conj blm hash)) bloom hashes))
 
(defn bloom-contains [bloom hashes]
  (all (map (fn [hash] (bloom hash)) hashes)))

(defn bloom-contains-word [bloom hasher word]
  (bloom-contains bloom (hasher word)))

(defn fill-bloom [hasher words]
  (reduce (fn [blm word] (bloom-add blm (hasher word))) (bloom-new) words))

(defn spellchecker [hasher]
  (with-open [file (io/reader "/usr/share/dict/words")]
    (fill-bloom hasher (line-seq file))))

(defn test-word [checker word]
  (do (print word)
      (print " is")
      (print (if (checker word) "" " not"))
      (println " a word.")))

(let [hasher (partial get-hashes "MD5" 3 19)
      is-a-word (partial bloom-contains-word (spellchecker hasher) hasher)]
  (do (test-word is-a-word "spoon")
      (test-word is-a-word "enunciate")
      (test-word is-a-word "sporn")
      (test-word is-a-word "spooon")
      (test-word is-a-word "perambulate")
      (test-word is-a-word "aisle")
      (test-word is-a-word "zephyr")
      (test-word is-a-word "neph")))

