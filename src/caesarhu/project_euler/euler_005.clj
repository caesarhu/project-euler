(ns caesarhu.project-euler.euler-005
  (:require [clojure.math.numeric-tower :refer [lcm]]))

(defn solve
  [n]
  (reduce lcm 1 (range 1 (inc n))))

(comment
  (time (solve 20))
  )
