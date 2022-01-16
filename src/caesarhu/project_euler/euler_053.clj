(ns caesarhu.project-euler.euler-053
  (:require [caesarhu.math.math-tools :refer [binomial]]))

(defn solve
  [x]
  (->> (for [n (range 1 (inc x))
             r (range 1 (inc n))
             :when (> (binomial n r) 1000000)]
         1)
       (apply +)))

(comment
  (time (solve 100))
  )

