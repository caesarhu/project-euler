(ns caesarhu.project-euler.euler-047
  (:require [caesarhu.primes :as p]
            [caesarhu.shun-tools.primes :as p2]
            [caesarhu.shun-tools.math-misc :as misc]))

(defn factors-count
  [n]
  (->> (p/factors n)
       distinct
       count))

(defn factors-count2
  [n]
  (->> (p2/prime-factors-of n)
       distinct
       count))

(defn consecutive?
  [v]
  (->> (partition 2 1 v)
       (map #(apply - %))
       (every? #(= -1 %))))

(defn solve
  []
  (let [numbers (->> (iterate inc 2)
                     (filter #(= 4 (factors-count %))))]
    (loop [nums numbers]
      (if (consecutive? (take 4 nums))
        (first nums)
        (recur (rest nums))))))

(comment
  (time (solve))
  )