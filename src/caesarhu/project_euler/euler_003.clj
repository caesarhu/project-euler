(ns caesarhu.project-euler.euler-003
  (:require [caesarhu.shun-tools.primes :as p]
            [caesarhu.shun-tools.math-misc :as misc]
            [clojure.math.numeric-tower :as math]
            [caesarhu.shun-tools.pollard-rho :as rho]
            [criterium.core :refer [bench]]))

(defn prime-factors-of
  [n]
  (let [sqr (first (math/exact-integer-sqrt n))]
    (loop [n n
           prime-seq p/primes
           result []]
      (let [p (first prime-seq)]
        (cond
          (or (> p sqr) (= n p)) (cons n result)
          (zero? (rem n p)) (recur (quot n p) prime-seq (cons p result))
          :else (recur n (next prime-seq) result))))))

(defn brute-force
  [n]
  (apply max (prime-factors-of n)))

(defn solve
  [n]
  (apply max (keys (rho/prime-factors n))))

(comment
  (time (solve 600851475143))
  (time (brute-force 600851475143))
  (time (rho/prime-factors 600851475143 rho/floyd))
  (bench (rho/prime-factors (*' 1238926361552897 93461639715357977769163558199606896584051237541638188580280321) rho/floyd))
  )