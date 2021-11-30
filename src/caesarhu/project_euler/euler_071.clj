(ns caesarhu.project-euler.euler-071
  (:require [clojure.math.numeric-tower :as math]))

(defn nearest-fraction
  [fraction n]
  (let [nearest (int (* n fraction))
        i (if (== nearest (* n fraction))
            (dec nearest)
            nearest)]
    (/ i n)))

(defn solve
  [limit]
  (->> (range 1 (inc limit))
       (map #(nearest-fraction 3/7 %))
       (apply max)
       numerator))
