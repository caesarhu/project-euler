(ns caesarhu.project-euler.euler-020
  (:require [caesarhu.shun-tools.math-misc :as misc]))

(defn factorial
  [n]
  (apply *' (range 1 (inc n))))

(defn solve
  [n]
  (->> (factorial n)
       (misc/digits)
       (apply +)))

(comment
  (time (solve 100))
  )