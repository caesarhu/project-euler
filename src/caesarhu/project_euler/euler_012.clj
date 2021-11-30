(ns caesarhu.project-euler.euler-012
  (:require [caesarhu.shun-tools.primes :refer [prime-factors-of]]
            [caesarhu.shun-tools.pollard-rho :as rho]))

(defn triangular
  [n]
  (quot (* n (inc n)) 2))

(def triangular-numbers
  (map triangular (iterate inc 2)))

(defn factors-of
  [n]
  (->> (prime-factors-of n)
       frequencies
       vals
       (map inc)
       (apply *)))

(defn brute-force
  [n]
  (some #(and (> (factors-of %) n) %) triangular-numbers))

(comment
  (time (brute-force 500))
  )