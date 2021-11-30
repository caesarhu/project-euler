(ns caesarhu.project-euler.euler-029
  (:require [clojure.math.numeric-tower :as math]))

(defn solve
  [n]
  (let [n-range (range 2 (inc n))]
    (->> (for [i n-range
               j n-range]
           (math/expt i j))
         (distinct)
         (count))))

; (time (solve 100))
