(ns caesarhu.project-euler.euler-115
  (:require [clojure.math.combinatorics :as comb]))

(defn block-combinations
  [m n]
  (apply + (for [i (range (inc (quot (inc n) (inc m))))]
             (comb/count-combinations (range (- (inc n) (* (dec m) i))) (* 2 i)))))

(defn euler-115
  [limit]
  (first (for [i (iterate inc 1)
               :let [b (block-combinations 50 i)]
               :when (> b limit)]
           i)))

; (time (euler-115 1000000))