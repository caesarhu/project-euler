(ns caesarhu.project-euler.euler-076
  (:require [clojure.math.combinatorics :as comb]))

(def count-partitions
  (memoize
    (fn [n m]
      (cond
        (or (< n 0) (zero? m)) 0
        (zero? n) 1
        :else (+ (count-partitions (- n m) m)
                 (count-partitions n (dec m)))))))

(defn solve [n]
  (count-partitions n (dec n)))

; (time (solve 100))
