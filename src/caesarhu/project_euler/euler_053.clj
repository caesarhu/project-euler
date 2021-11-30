(ns caesarhu.project-euler.euler-053
  (:require [clojure.math.combinatorics :as comb]))

(defn greater-million?
  [n r]
  (> (comb/count-combinations (range n) r)
     1000000))

(defn solve
  []
  (->> (for [n (range 1 101)
             r (range 1 (inc n))
             :when (greater-million? n r)]
         1)
       flatten
       (apply +)))

(comment
  (time (solve))
  )

