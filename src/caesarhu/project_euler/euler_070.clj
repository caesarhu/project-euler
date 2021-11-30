(ns caesarhu.project-euler.euler-070
  (:require [caesarhu.shun-tools.math-misc :as misc]
            [caesarhu.shun-tools.primes :as p]
            [clojure.math.numeric-tower :as math]))

(defn permutation?
  [a b]
  (= (frequencies (misc/digits a))
     (frequencies (misc/digits b))))

(defn totient-permutation?
  [n]
  (permutation? n (p/phi n)))

(defn solve
  [limit]
  (let [prime-seq (p/primes-range (math/expt 10 3) (math/expt 10 4))]
    (->> (for [p prime-seq
               :let [p1-limit (/ limit p)]
               p1 (take-while #(< % p1-limit) prime-seq)
               :let [product (* p p1)]
               :when (totient-permutation? product)]
           [product (/ product (p/phi product))])
         (apply min-key last))))
