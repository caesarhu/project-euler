(ns caesarhu.project-euler.euler-023
  (:require [caesarhu.shun-tools.math-misc :as misc]))

(defn divisors-sum
  [n]
  (->> (misc/divisors n)
       (drop-last 1)
       (apply +)))

(def abundant?
  (memoize
    (fn [n]
      (> (divisors-sum n) n))))

(def abundants (filter abundant? (range 12 28124)))
(def abundant-set
  (set abundants))

(defn sum-of-abundant?
  [n]
  (let [limit (inc (/ n 2))
        possible-abundants (take-while #(< % limit) abundants)]
    (some #(abundant-set (- n %)) possible-abundants)))

(defn solve
  [n]
  (->> (filter (complement sum-of-abundant?) (range 1 (inc n)))
       (apply +)))

(comment
  (time (solve 28183))
  )

