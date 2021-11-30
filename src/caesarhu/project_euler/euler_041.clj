(ns caesarhu.project-euler.euler-041
  (:require [caesarhu.shun-tools.primes :as p]
            [clojure.math.combinatorics :as comb]
            [caesarhu.shun-tools.math-misc :as misc]))

(defn pandigital-digits
  [n]
  (reverse (range 1 (inc n))))

(defn max-pandigital-prime
  [n]
  (->> (comb/permutations (pandigital-digits n))
       (map misc/to-number)
       (some #(and (p/is-prime? %) %))))

(defn brute-force
  []
  (->> (map #(max-pandigital-prime %) (pandigital-digits 9))
       (remove nil?)
       first))

(defn solve
  []
  (max-pandigital-prime 7))

(comment
  (time (solve))
  "因為 2、3、5、6、8、9位數 pandigital 一定可以被3整除，所以只有4、7位數有可能有 pandigital 質數"
  )