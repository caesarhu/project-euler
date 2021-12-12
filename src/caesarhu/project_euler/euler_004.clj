(ns caesarhu.project-euler.euler-004
  (:require [clojure.math.numeric-tower :as math]
            [clojure.math.combinatorics :as combo]
            [caesarhu.shun-tools.math-misc :as misc]))

(defn palindrome-generator
  [n]
  (let [first-digit (range 9 0 -1)
        other-digit (range 9 -1 -1)]
    (->> (cons first-digit (repeat (dec n) other-digit))
         (apply combo/cartesian-product)
         (map (fn [coll]
                (concat coll (reverse coll))))
         (map misc/to-number))))

(defn valid-digits?
  [d n]
  (let [valid? (fn [x] (>= (dec (math/expt 10 d)) x (math/expt 10 (dec d))))
        divisors (misc/divisors n)]
    (some #(valid? (quot n %)) (filter valid? divisors))))

(defn solve
  [d]
  (some #(and (valid-digits? d %) %) (palindrome-generator d)))

(comment
  (time (solve 3))
  )
