(ns caesarhu.project-euler.euler-049
  (:require [caesarhu.shun-tools.primes :as p]
            [caesarhu.shun-tools.math-misc :as misc]
            [clojure.math.combinatorics :as comb]))

(defn ->digits
  [n]
  {(sort (misc/digits n)) [n]})

(defn arithmetic?
  [coll]
  (->> (comb/combinations coll 3)
       (filter (fn [[a b c]]
                 (= (+ b b) (+ a c))))))

(defn solve
  []
  (->> (p/primes-range 1000 10000)
       (map ->digits)
       (apply merge-with concat)
       (map last)
       (filter #(>= (count %) 3))
       (map arithmetic?)
       (remove empty?)
       (apply concat)
       (map #(apply str %))
       (map #(Long/parseLong %))))

(comment
  (time (solve))
  )

