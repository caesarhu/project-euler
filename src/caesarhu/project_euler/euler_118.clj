(ns caesarhu.project-euler.euler-118
  (:require [clojure.math.combinatorics :as comb]
            [caesarhu.shun-tools.math-misc :as misc]
            [caesarhu.shun-tools.primes :as p]))

(defn seq-primes
  [nv]
  (->> (comb/permutations nv)
       (map misc/to-number)
       (filter p/is-prime?)))

(defn part-prime-set
  [partitions]
  (loop [parts (sort-by count partitions)
         result []]
    (let [prime-set (seq-primes (first parts))]
      (cond
        (empty? parts) result
        (empty? prime-set) []
        :else (recur (rest parts) (conj result prime-set))))))

(defn merge-parts
  [parts]
  (apply comb/cartesian-product parts))

(defn count-sets
  [n]
  (->> (comb/partitions (range 1 (inc n)))
       (map part-prime-set)
       (filter not-empty)
       (map merge-parts)
       (map count)
       (apply +)))

(defn euler-118
  []
  (count-sets 9))
