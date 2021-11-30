(ns caesarhu.project-euler.euler-048
  (:require [clojure.math.numeric-tower :as math]))

(defn solve
  [n]
  (->> (range 1N (inc n))
       (map #(math/expt % %))
       (apply +)
       (str)
       (take-last 10)
       (apply str)))

(comment
  (time (solve 1000))
  )