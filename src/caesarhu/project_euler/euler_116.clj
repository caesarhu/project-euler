(ns caesarhu.project-euler.euler-116
  (:require [clojure.math.combinatorics :as comb]))

(def red 2)
(def green 3)
(def blue 4)

(defn block-permutations
  [m n]
  (let [length (quot m n)]
    (apply + (for [i (range 1 (inc length))
                   :let [i2 (repeat i 2)
                         m1-length (- m (* i n))
                         m1 (repeat m1-length 1)]]
               (comb/count-permutations (concat i2 m1))))))

(defn rgb-permutations
  [n]
  (+ (block-permutations n red)
     (block-permutations n green)
     (block-permutations n blue)))
