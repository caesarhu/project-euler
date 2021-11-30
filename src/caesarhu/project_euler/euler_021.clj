(ns caesarhu.project-euler.euler-021
  (:require [caesarhu.shun-tools.math-misc :as misc]))

(defn divisors-sum
  [n]
  (->> (misc/divisors n)
       (drop-last 1)
       (apply +)))

(defn amicable?
  [n]
  (let [sum-1 (divisors-sum n)
        sum-2 (divisors-sum sum-1)]
    (and (> n 1)
         (= n sum-2)
         (not= n sum-1))))

(defn solve
  [n]
  (->> (range 1 (inc n))
       (filter amicable?)
       (apply +)))

(comment
  (time (solve 10000))
  )