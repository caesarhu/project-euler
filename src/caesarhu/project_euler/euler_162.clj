(ns caesarhu.project-euler.euler-162
  (:require [caesarhu.shun-tools.math-misc :as misc]))

(def hex-base 16)
(def hex-vector ['0 '1 '2 '3 '4 '5 '6 '7 '8 '9 'A 'B 'C 'D 'E 'F])
(def target-digits [0 1 10])

(defn to-hex
  [n]
  (if (zero? n) [0]
      (misc/digits n hex-base)))

(defn hex-str
  [n]
  (->> (map hex-vector (to-hex n))
       (apply str)))

(defn valid-hex?
  ([n target]
   (let [hex-set (set (to-hex n))]
     (every? hex-set target)))
  ([n]
   (valid-hex? n target-digits)))

(defn brute-force
  ([hex-digits]
   (brute-force hex-digits target-digits))
  ([hex-digits target]
   (let [limit (misc/to-number (cons 1 (repeat hex-digits 0)) hex-base)]
     (count (filter #(valid-hex? % target) (range limit))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; https://projecteuler.net/thread=162 bunuelo's solution

(defn solve
  [hex-digits]
  (loop [hex-digits hex-digits
         [n a b c d] [0 0 0 0 1]]
    (if (pos-int? hex-digits)
      (recur (dec hex-digits) [(+' n (*' (- hex-base 3) a) (*' 2 b))
                               (+' (*' (- hex-base 3) a) (*' 3 b))
                               (+' (*' (- hex-base 2) b) (*' 2 c))
                               (+' (*' (dec hex-base) c) d)
                               (*' hex-base d)])
      (hex-str n))))

(defn euler-162
  []
  (time (solve 16)))