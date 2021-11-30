(ns caesarhu.project-euler.euler-035
  (:require [caesarhu.shun-tools.math-misc :as misc]
            [caesarhu.shun-tools.miller-rabin :refer [deterministic-test]]
            [caesarhu.shun-tools.primes :as p]))

(defn rotate
  [coll]
  (concat (rest coll) [(first coll)]))

(defn rotations
  [n]
  (let [digits (misc/digits n)]
    (->> (iterate rotate digits)
         (take (count digits))
         (map misc/to-number))))

(defn circular-prime?
  [p]
  (every? p/is-prime? (rotations p)))

(defn solve
  [limit]
  (->> (p/primes-range 1 limit)
       (filter circular-prime?)
       count))

(comment
  (circular-prime? 1234)
  (time (solve 1000000))
  )