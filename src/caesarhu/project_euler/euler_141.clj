(ns caesarhu.project-euler.euler-141
  (:require [caesarhu.shun-tools.primes :as p]
            [clojure.math.numeric-tower :as math]
            [caesarhu.shun-tools.math-misc :as misc]))

(defn generate-square
  [a b c]
  (+' (*' a a a b c c)
      (*' b b c)))

(defn euler-141
  [limit]
  (let [n-limit (first (drop-while #(< (* % % %) limit) (iterate inc 2)))]
    (->> (for [a (range 2 n-limit)
               b (range 1 a)
               :when (and (p/coprime? a b)
                          (< (+' (*' a a a b b) (*' b b)) limit))
               c (take-while #(< (generate-square a b %) limit) (iterate inc 1))
               :let [n (generate-square a b c)]
               :when (misc/is-square? n)]
           n)
         set
         (apply +))))
