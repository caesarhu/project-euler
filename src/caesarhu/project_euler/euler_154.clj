(ns caesarhu.project-euler.euler-154
  (:require [caesarhu.shun-tools.math-misc :as misc]
            [clojure.math.combinatorics :as combo]))

(def target 200000)

(defn prime-power-of-n!
  [n p]
  (quot (- n (apply + (misc/digits n p))) (dec p)))

(defn digit-combinator
  "digit - digit number
   base - prime base 2 or 5
   carry-in - 0 or 1
   carry-out - 0 or 1"
  [digit base carry-in carry-out]
  (let [sum (+ (* carry-out base) (- digit carry-in))]
    (for [i (range (min base (inc sum)))
          :let [d-i (- sum i)]
          :when (< d-i base)]
      [i d-i])))

(defn number-combinator
  [n p carry-out-seq]
  (let [digits (misc/digits n p)
        carry-in-seq (concat (rest carry-out-seq) [0])]
    (map digit-combinator digits (repeat p) carry-in-seq carry-out-seq)))

(defn count-number-combinator
  [n p carry-out-seq]
  (->> (number-combinator n p carry-out-seq)
       (map count)
       (apply *)))

(defn number-combinator-seq
  [n p carry-out-seq]
  (let [power (apply + carry-out-seq)]
    (->> (number-combinator n p carry-out-seq)
         (apply combo/cartesian-product)
         (map #(map first %))
         (map #(vector power (misc/to-number % p))))))

(comment
  (number-combinator-seq target 5 [0 1 1 1 1 1 1 1])
  )