(ns caesarhu.project-euler.euler-157
  (:require [clojure.math.numeric-tower :as math]
            [caesarhu.shun-tools.math-misc :as misc]))

(defn brute-force
  [n]
  (let [n2 (* n n)
        ds (misc/divisors n2)]
    (set (for [d ds
               :let [base-a (+ d n)
                     base-b (+ (quot n2 d) n)
                     g (math/gcd base-a base-b)]
               p (misc/divisors g)]
           (sort [(quot base-a p) (quot base-b p)])))))

(defn solve
  [limit]
  (apply + (map #(count (brute-force %)) (for [i (range 1 (inc limit))]
                                           (math/expt 10 i)))))

(defn euler-157
  []
  (time (solve 9)))

; https://math.stackexchange.com/questions/183890/how-many-integer-solutions-to-a-diophantine-equation
