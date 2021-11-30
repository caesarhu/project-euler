(ns caesarhu.project-euler.euler-117
  (:require [clojure.math.combinatorics :as comb]))

(defn block-permutations
  [m colors]
  (let [c-v (for [c colors
                  :let [length (quot m c)]]
              (for [cs (map #(repeat % c) (range (inc length)))]
                cs))
        merge-grey (fn [coll]
                     (let [sum (apply + coll)
                           length (- m sum)]
                       (concat coll (repeat length 1))))]
    (->> (apply comb/cartesian-product c-v)
         (map #(apply concat %))
         (filter #(<= (apply + %) m))
         (map merge-grey)
         (map comb/count-permutations)
         (apply +))))

(defn euler-117
  [n]
  (block-permutations n [2 3 4]))

; (time (euler-117 50))
