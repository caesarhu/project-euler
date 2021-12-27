(ns caesarhu.project-euler.euler-043
  (:require [caesarhu.shun-tools.math-misc :as misc])
  (:import [com.google.ortools.sat CpModel IntVar CpSolverSolutionCallback LinearExpr]
           [com.google.ortools.util Domain]))

(defn add-digit-divisible
  [v p]
  (let [v-set (set v)]
    (for [i (range 10)
          :when (not (v-set i))
          :let [vi (cons i v)
                t (take 3 vi)]
          :when (zero? (mod (misc/to-number t) p))]
      vi)))

(defn filter-rules
  [primes]
  (let [init (map vector (range 10))]
    (reduce (fn [acc p]
              (mapcat #(add-digit-divisible % p) acc))
            init primes)))

(defn solve
  []
  (let [primes [1 17 13 11 7 5 3 2 1]]
    (->> (filter-rules primes)
         (map misc/to-number)
         (apply +))))

(comment
  (filter-rules [1 17 13])
  (time (solve))
  )