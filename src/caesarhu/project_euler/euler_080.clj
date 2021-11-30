(ns caesarhu.project-euler.euler-080
  (:require [caesarhu.shun-tools.math-misc :as misc]))

(defn number-pairs [n]
  (let [join (fn [[a b]] (+ (* 10 a) b))
        d (misc/digits n)
        normalized (if (odd? (count d)) (cons 0 d) d)]
    (concat
      (map join (partition 2 normalized))
      (repeat 0))))

(defn calc-y [x p]
  (* x (+ (* 20 p) x)))

(defn biggest-x [c p]
  (->> (iterate inc 0)
       (take-while #(<= (calc-y % p) c))
       last))

(defn- sqrt0 [xs r p]
  (let [c (+ (* r 100) (first xs))
        x (biggest-x c p)
        y (calc-y x p)
        r (- c y)
        p (+ (* 10 p) x)]
    (lazy-seq
      (if (and (zero? c) (zero? r))
        nil
        (cons x (sqrt0 (next xs) r p))))))

(defn sqrt-digits-seq [n]
  (let [xs (number-pairs n)]
    (sqrt0 xs 0N 0N)))

(defn irrational-roots [limit]
  (->> (range 1 (inc limit))
       (remove misc/is-square?)))

(defn solve [n digits]
  (->> (irrational-roots n)
       (mapcat #(take digits (sqrt-digits-seq %)))
       (reduce +)))

; (time (solve 100 100))