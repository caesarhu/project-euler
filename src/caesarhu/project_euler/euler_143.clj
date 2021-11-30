(ns caesarhu.project-euler.euler-143
  (:require [clojure.math.numeric-tower :as math]
            [caesarhu.shun-tools.primes :as p]
            [caesarhu.shun-tools.math-misc :as misc]
            [clojure.math.combinatorics :as comb]))

(defn generate-fermat
  [limit]
  (let [fermat-map (->> (for [u (range 1 (inc (first (math/exact-integer-sqrt limit))))
                              v (range 1 u)
                              :when (and (p/coprime? u v)
                                         (not (zero? (mod (- u v) 3))))
                              :let [a (+ (* 2 u v) (* v v))
                                    b (- (* u u) (* v v))]
                              :when (< (+ a b) limit)
                              k (range 1 (/ limit (+ a b)))
                              :let [ak (* a k)
                                    bk (* b k)]]
                          {ak #{bk} bk #{ak}})
                        (apply merge-with clojure.set/union))
        match-third (fn [[k vset]]
                      (for [[x y] (comb/combinations vset 2)
                            :when ((fermat-map x) y)]
                        (+ k x y)))]
    (->> (map match-third fermat-map)
         (remove empty?)
         flatten
         (remove #(> % limit))
         distinct)))

(defn euler-143
  [limit]
  (->> (generate-fermat limit)
       (apply +)))
