(ns caesarhu.project-euler.euler-097
  (:require [clojure.math.numeric-tower :as math]
            [caesarhu.shun-tools.math-misc :as misc]))

(defn power-mod
  [base power divisor]
  (let [m* (fn [p q] (mod (*' p q) divisor))]
    (loop [base base power power result 1]
      (cond
        (zero? power) result
        (even? power) (recur (m* base base) (quot power 2) result)
        :else (recur (m* base base) (quot power 2) (m* base result))))))

(defn solve
  []
  (->> (inc (*' 28433 (power-mod 2 7830457 (math/expt 10 10))))
       misc/digits
       (take-last 10)
       (misc/to-number)))

(comment
  (time (solve))
  )