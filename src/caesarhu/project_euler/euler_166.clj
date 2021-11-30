(ns caesarhu.project-euler.euler-166
  (:require [clojure.math.combinatorics :as comb]))

; https://projecteuler.net/thread=166;page=8
; oozk's solution

(defn count-grid
  ([[a b c d e f g h] s]
   (let [dset (set (range 10))
         valid-x? (fn [x] (every? dset [(- s x a e) (- s x b g) (- (+ x a e) f c)]))
         valid-y? (fn [y] (every? dset [(- s y a h) (- s y b f) (- (+ y a h) g c)]))
         count-x (fn [x] (if (valid-x? x)
                           (count (filter valid-y? (range 10)))
                           0))]
     (transduce (map count-x)
                + 0 (range 10))))
  ([[a b c d e f g h]]
   (count-grid [a b c d e f g h] (+ a b c d))))

(defn oozk
  []
  (let [drange (range 10)
        dset (set drange)]
    (apply + (for [a drange
                   b drange
                   c drange
                   d drange
                   :let [s (+ a b c d)]
                   e drange
                   f drange
                   :let [g (- s b c f)] :when (dset g)
                   :let [h (- s a d e)] :when (and (dset h)
                                                   (= s (+ e f g h)))]
               (count-grid [a b c d e f g h] s)))))

; my solution, base on oozk's solution
(comment
  "d1 a1 a2 d5
   c1 d2 d6 e1
   c2 d7 d3 e2
   d8 b1 b2 d4"
  "S = d1 + d2 + d3 + d4 = d5 + d6 + d7 + d8 = c1 + d2 + d6 + e1 = c2 + d7 + d3 + e2
     = d1 + c1 + c2 + d8 = d5 + e1 + e2 + d4
   d2 + d6 + d7 + d3 = S - ( c1 + c2 ) + S - ( e1 + e2 ) = d1 + d8 + d5 + d4
   2S = d1 + d2 + d3 + d4 + d5 + d6 + d7 + d8
      = 2 ( d2 + d6 + d7 + d3 )
   S = d2 + d6 + d7 + d3 = d1 + d8 + d5 + d4"
  "所以，給定 d1, d2, d3, d4, d5, d6 6個變數，可求得 d7, d8，
   再代入 a1, c1，可求得 grid"
  "若事先設定 S，則只需5個變數"
  )

(defn solve
  []
  (let [drange (range 10)
        dset (set drange)
        abcdef (apply comb/cartesian-product (repeat 6 drange))
        ->gh (fn [[a b c d e f]]
               (let [s (+ a b c d)
                     g (- s b c f)
                     h (- s a d e)
                     t (+ e f g h)]
                 (when (and (dset g) (dset h) (= s t))
                   [a b c d e f g h])))]
    (transduce (comp
                (map ->gh)
                (remove nil?)
                (map count-grid))
               + 0 
               abcdef)))

(defn euler-166
  []
  (time (solve)))