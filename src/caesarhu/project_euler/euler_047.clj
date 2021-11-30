(ns caesarhu.project-euler.euler-047
  (:require [caesarhu.shun-tools.primes :as p]
            [caesarhu.shun-tools.math-misc :as misc]))

(defn prime-factor-4?
  [n]
  (->> (p/prime-factors-of n)
       distinct
       count
       (#(= 4 %))))

(defn consecutive?
  [coll]
  (->> (partition 2 1 coll)
       (map #(apply - %))
       (apply = -1)))

(defn solve
  []
  (->> misc/integers
       (filter prime-factor-4?)
       (partition 4 1)
       (filter consecutive?)
       ffirst))

(comment
  (time (solve))
  )