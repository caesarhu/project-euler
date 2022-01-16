(ns caesarhu.project-euler.euler-003
  (:require [caesarhu.math.primes :as p]
            [clojure.math.numeric-tower :as math]))

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

(comment
  (time (brute-force 600851475143))
  )