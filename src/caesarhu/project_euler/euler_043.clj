(ns caesarhu.project-euler.euler-043
  (:require [caesarhu.shun-tools.math-misc :as misc]
            [caesarhu.shun-tools.primes :as p]
            [clojure.math.combinatorics :as comb]
            [criterium.core :refer [bench]]))

(def pandigital-seq
  (comb/permutations (range 0 10)))

(defn slices
  [xs]
  (let [get-slice (fn [n] (misc/to-number (take 3 (drop n xs))))]
    (map get-slice (range 1 8))))

(defn is-substring-divisible? [xs]
  (every? zero? (map mod (slices xs) p/primes)))

(defn calc 
  [coll]
  (->> coll
       (filter is-substring-divisible?)
       (map misc/to-number)
       (reduce +)))

(defn brute-force 
  []
  (->> pandigital-seq
       (partition-all 20000)
       (pmap calc)
       (reduce +)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn possible
  [p coll]
  (let [s (set coll)
        distinct-digit? (fn [d] (not (s d)))
        tail-2-digits (vec (take-last 2 coll))
        valid-p? (fn [coll] (zero? (mod (misc/to-number coll) p)))
        valid-digit? (fn [d] (and (distinct-digit? d)
                                  (valid-p? (conj tail-2-digits d))))]
    (for [d (range 10)
          :when (valid-digit? d)]
      (conj coll d))))

(defn combine-possible
  [coll p]
  (apply concat (map #(possible p %) coll)))

(defn solve
  []
  (loop [primes-seq [2 3 5 7 11 13 17]
         digits-seq (for [i (range 100 1000)
                          :let [ds (misc/digits i)]
                          :when (apply distinct? ds)]
                      (vec ds))]
    (if (empty? primes-seq)
      (apply + (map misc/to-number digits-seq))
      (recur (rest primes-seq) (combine-possible digits-seq (first primes-seq))))))

(comment
  (time (solve))
  (time (brute-force))
  )