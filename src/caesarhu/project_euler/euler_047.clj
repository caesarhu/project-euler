(ns caesarhu.project-euler.euler-047
  (:require [caesarhu.primes :as p]))

(defn factors-count
  [n primes]
  (->> (p/factors n primes)
       distinct
       count))

(defn consecutive?
  [v]
  (->> (partition 2 1 v)
       (map #(apply - %))
       (every? #(= -1 %))))

(defn solve
  []
  (let [primes (p/primes-tox 1000)
        numbers (->> (iterate inc 2)
                     (filter #(= 4 (factors-count % primes))))]
    (loop [nums numbers]
      (if (consecutive? (take 4 nums))
        (first nums)
        (recur (rest nums))))))

(comment
  (time (solve))
  )