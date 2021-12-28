;; EULER #059
;; ==========
;; Each character on a computer is assigned a unique code and the preferred
;; standard is ASCII (American Standard Code for Information Interchange).
;; For example, uppercase A = 65, asterisk (*) = 42, and lowercase k = 107.
;;
;; A modern encryption method is to take a text file, convert the bytes
;; to ASCII, then XOR each byte with a given value, taken from a secret
;; key. The advantage with the XOR function is that using the same
;; encryption key on the cipher text, restores the plain text; for
;; example, 65 XOR 42 = 107, then 107 XOR 42 = 65.
;;
;; For unbreakable encryption, the key is the same length as the plain
;; text message, and the key is made up of random bytes. The user would
;; keep the encrypted message and the encryption key in different
;; locations, and without both "halves", it is impossible to decrypt
;; the message.
;;
;; Unfortunately, this method is impractical for most users, so the
;; modified method is to use a password as a key. If the password is
;; shorter than the message, which is likely, the key is repeated
;; cyclically throughout the message. The balance for this method is
;; using a sufficiently long password key for security, but short enough
;; to be memorable.
;;
;; Your task has been made easy, as the encryption key consists of three
;; lower case characters. Using 'data/cipher1.txt', a file containing the
;; encrypted ASCII codes, and the knowledge that the plain text must
;; contain common English words, decrypt the message and find the sum of
;; the ASCII values in the original text.
;;

(ns caesarhu.project-euler.euler-059
  (:require [clojure.math.combinatorics :as comb]))

(def fname "resources/data/p059_cipher.txt")

(defn get-encrypted-bytes [fname]
  (map read-string (clojure.string/split (slurp fname) #",")))

(defn char->int [c]
  (int c))

(defn int->char
  [n]
  (char n))

(defn build-score-map [] ;; See https://en.wikipedia.org/wiki/Etaoin_shrdlu
  (let [freq-phrase (apply str (reverse "ETAOIN SHRDLU"))
        upper-freqs (map vector freq-phrase (rest (range)))
        lower-freqs (map vector (.toLowerCase freq-phrase) (rest (range)))]
    (into {} (concat upper-freqs lower-freqs))))

(defn bytes-score
  [score-map bytes mask]
  (->> (map bit-xor bytes (cycle mask))
       (map (comp score-map int->char))
       (remove nil?)
       (reduce +)))

(defn solve []
  (let [input (get-encrypted-bytes fname)
        score-map  (build-score-map)
        char-range (range (char->int \a) (inc (char->int \z)))
        masks      (for [a char-range b char-range c char-range] [a b c])
        mask       (apply max-key (partial bytes-score score-map input) masks)
        result (map bit-xor input (cycle mask))]
    {:sum (apply + result)}
    (->> (map bit-xor input (cycle mask))
         (map int->char)
         (apply str))))

(comment
  (time (solve))
  )
