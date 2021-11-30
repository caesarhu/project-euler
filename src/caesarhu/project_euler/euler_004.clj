(ns caesarhu.project-euler.euler-004
  (:require [caesarhu.shun-tools.pollard-rho :as rho]
            [clojure.math.combinatorics :as comb]
            [caesarhu.shun-tools.math-misc :as misc]))


(defn palindrome?
  [coll]
  (= coll (reverse coll)))

(defn palindrome-number?
  [n]
  (palindrome? (seq (str n))))

(defn brute-force
  [digits]
  (let [start (apply * (repeat (dec digits) 10))
        end (apply * (repeat digits 10))]
    (->> (for [i (range start end)
               j (range (inc i) end)
               :let [sum (* i j)]
               :when (palindrome-number? sum)]
           sum)
         (apply max))))

(defn solve
  [digits]
  (let [start (apply * (repeat (dec digits) 10))
        end (apply * (repeat digits 10))
        valid-digits? (fn [n] (and (>= n start) (< n end)))
        valid-number? (fn [n]
                        (first (for [d (rho/divisors n)
                                     :when (valid-digits? d)
                                     :let [q (quot n d)]
                                     :when (valid-digits? q)]
                                 n)))]
    (first (for [n (range (misc/to-number (repeat (* 2 digits) 9)) (misc/to-number (repeat (dec (* 2 digits)) 9)) -1)
                 :when (and (palindrome-number? n)
                            (valid-number? n))]
             n))))

(comment
  (time (solve 3))
  (time (brute-force 3))
  (take 100 (filter palindrome-number? (range 999999 99999 -1)))
  )
