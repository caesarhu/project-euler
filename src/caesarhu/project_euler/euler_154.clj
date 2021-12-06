(ns caesarhu.project-euler.euler-154
  (:require [caesarhu.shun-tools.math-misc :as misc]
            [clojure.math.combinatorics :as combo]
            [clojure.set :refer [intersection]]))

(def target 200000)

(defn generate-power-map
  [^long limit]
  (let [f2 (atom [0])
        f5 (atom [0])]
    (doseq [i (range 1 (inc limit))
            :let [i2 (quot i 2)
                  i5 (quot i 5)
                  n2 (@f2 i2)
                  n5 (@f5 i5)]]
      (swap! f2 assoc i (+ n2 i2))
      (swap! f5 assoc i (+ n5 i5)))
    {2 @f2 5 @f5}))

(def default-power-map
  (generate-power-map target))

(defn prime-power-of-n!
  [n p]
  ((default-power-map p) n))

(defn prime-power-i-of-n!
  [n i p]
  (apply - (map #(prime-power-of-n! % p) [n i (- n i)])))

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
  (sequence (comp
             (map #(map first %))
             (map #(misc/to-number % p)))
            (->> (number-combinator n p carry-out-seq)
                 (apply combo/cartesian-product))))

(defn generate-carry
  [n prime power]
  (let [max-carry (-> (misc/digits n prime) count dec)]
    (when (>= max-carry power)
      (->> (for [i (range power (inc max-carry))]
             (let [carry-seq (concat (repeat (- max-carry i) 0) (repeat i 1))]
               (map #(cons 0 %) (combo/permutations carry-seq))))
           (apply concat)))))

(defn count-prime-power
  [n p power]
  (->> (map #(count-number-combinator n p %) (generate-carry n p power))
       (apply +)))

(defn prime-power-seq
  [n p power]
  (mapcat #(number-combinator-seq n p %) (generate-carry n p power)))

(defn count-power-of-10
  [target m power-of-10]
  (let [power-2 (- power-of-10 (prime-power-i-of-n! target m 2))
        power-5 (- power-of-10 (prime-power-i-of-n! target m 5))]
    (if (<= power-2 0)
      (count-prime-power m 5 power-5)
      (->> (prime-power-seq m 5 power-5)
           (map #(prime-power-i-of-n! m % 2))
           (filter #(>= % power-2))
           count))))

(defn solve
  [limit power-of-10]
  (->> (range (inc limit))
       (map #(count-power-of-10 limit % power-of-10))
       (apply +)))

(comment
  (time (solve 2000 2))
  )