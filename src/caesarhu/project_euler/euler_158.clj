(ns caesarhu.project-euler.euler-158
  (:require [clojure.math.combinatorics :as comb]))

(defn count-left
  [coll]
  (loop [coll coll
         current 100
         counter 0]
    (if (empty? coll)
      counter
      (recur (rest coll) (first coll) (if (< current (first coll))
                                        (inc counter)
                                        counter)))))

(defn sum-left
  [coll]
  (->> (map count-left coll)
       (filter (partial = 1))
       count))

(defn sum-try
  [limit]
  (for [i (range 2 (inc limit))]
    (sum-left (comb/permutations (range i)))))

; https://oeis.org/A000295

(defn permutations-left-seq
  [limit]
  (reductions (fn [acc n]
                (dec (+ (* 2 acc) n)))
              (range limit)))

(defn solve
  []
  (let [chars 26
        left-vec (vec (permutations-left-seq (inc chars)))
        str-combinations (fn [n] (comb/count-combinations (range chars) n))
        sum-chars-left (fn [n] (* (str-combinations n) (get left-vec n)))]
    (apply max (map sum-chars-left (range 2 (inc chars))))))

(defn euler-158
  []
  (time (solve)))