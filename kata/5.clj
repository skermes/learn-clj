(require '[clojure.java.io :as io]
         '[clojure.string :as string])
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

(defn random-word [len]
  (let [letters "abcdefghijklmnopqrstuvwxyz"]
    (string/join (map (fn [x] (rand-nth letters)) (range len)))))

(def bad-word-list '("ywxqq" "mfqtf" "ldxbb" "ilccd" "gxehh" "clycx" "ctugb" "guqfh" "sryfs" "krkrb" "zituh" "iwcch" "nmuxd" "xywjo" "rbrep" "gyfkt" "qmscc" "xnmty" "yejix" "iesod" "yhhjv" "xpxfv" "zbhby" "nihaj" "wsohe" "xolhh" "axvua" "bepqp" "ebzdm" "gwymd" "zhfjg" "rijnb" "jfwzq" "xrbzw" "abuxb" "obptn" "rvdgt" "attqq" "drowp" "emtac" "bqclo" "ayafu" "mfgmd" "ntqaa" "ooxie" "djyhl" "qxtyp" "kjrel" "ypoph" "nnguv" "qwiqu" "rwbdy" "nusks" "zsuiz" "nxxgd" "iibro" "sedsq" "slces" "vusol" "gjhcy" "vgkhq" "apjhn" "dkjhw" "ghaaf" "blwla" "rbuhi" "egcsq" "kpyvf" "ttdku" "wrdjm" "tsyia" "ldyax" "zersx" "lloya" "slngl" "flgmt" "lhjix" "xishw" "knxrp" "afjpx" "ipnjb" "nlqbt" "kkbpd" "zdbkg" "otsds" "mgsry" "courc" "jwziq" "jvqnq" "twknh" "zmsam" "kuumr" "srsoj" "zvthv" "sahbj" "jmjjw" "sarot" "xwpyz" "ixmzd" "jkzar"))

(defn get-results [is-a-word]
  (reduce (fn [results word] (if (is-a-word word)
                                 (assoc results :words (conj (results :words) word))
                                 (assoc results :failcount (+ 1 (results :failcount)))))
          (hash-map :words [] :failcount 0)
          bad-word-list))

(defn test-hasher [strategy n-hashes hash-len]
  (do (println "Time for testing" n-hashes hash-len "-bit hashes from" strategy)
      (let [results (time (let [hasher (partial get-hashes strategy n-hashes hash-len)
                                is-a-word (partial bloom-contains-word (spellchecker hasher) hasher)]
                            (get-results is-a-word)))]
        (println "Accuracy:" (results :failcount) "out of 100."))
      (println)))

(doseq [n-hashes [3 4]
        hash-len [19 20 21 22]
        strategy ["MD5" "SHA1"]]
  (test-hasher strategy n-hashes hash-len))

