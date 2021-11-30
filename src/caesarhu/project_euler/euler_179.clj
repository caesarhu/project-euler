(ns caesarhu.project-euler.euler-179
  (:require [caesarhu.shun-tools.primes :as p]
            [caesarhu.shun-tools.math-misc :as misc]
            [clojure.math.numeric-tower :as math]
            [clojure.math.combinatorics :as combo]
            [caesarhu.shun-tools.pollard-rho :as rho]
            [injest.classical :refer [x>> =>>]]
            [net.cgrand.xforms :as x]))

(defn brute-force
  [limit]
  (->> (range 2 limit)
       (map misc/count-divisors)
       (partition 2 1)
       (filter (fn [[a b]] (= a b)))
       count))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn solve
  [limit]
  (let [divisors (atom (vec (repeat limit 1)))
        result (atom 0)]
    (doseq [i (range 2 limit)
            j (range i limit i)]
      (swap! divisors assoc j (inc (@divisors j))))
    (doseq  [i (range 2 (dec limit))]
      (when (= (@divisors i) (@divisors (inc i)))
        (swap! result inc)))
    @result))

(comment
  (time (solve (math/expt 10 7)))
  (time (brute-force (math/expt 10 7)))
  )