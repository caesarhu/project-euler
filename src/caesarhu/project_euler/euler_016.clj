(ns caesarhu.project-euler.euler-016
  (:require [clojure.math.numeric-tower :as math]
            [caesarhu.shun-tools.math-misc :as misc]))

(defn brute-force
  [n]
  (->> (math/expt 2 n)
       (misc/digits)
       (apply +)))

(defn power
  [n p]
  (loop [n n
         p p
         result 1]
    (if (<= p 0)
      result
      (if (zero? (bit-and 1 p))
        (recur (*' n n) (bit-shift-right p 1) result)
        (recur (*' n n) (bit-shift-right p 1) (*' result n))))))

(defn solve
  [p]
  (->> (power 2 p)
       (misc/digits)
       (apply +)))

(comment
  (power 2 1000)
  (time (solve 1000))
  (time (brute-force 1000))
  )