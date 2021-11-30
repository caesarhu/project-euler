(ns caesarhu.project-euler.euler-147
  (:require [caesarhu.shun-tools.math-misc :as misc]
            [clojure.math.combinatorics :as comb]))

(defn sub-rectangles
  [m n]
  (* (comb/count-combinations (range (inc m)) 2)
     (comb/count-combinations (range (inc n)) 2)))

(defn sub-diagonal-rectangles
  [m n]
  (let [y (max m n)
        x (min m n)]
    (+ (/ (* (dec x) x (+ 3 (* 4 x) (* 4 x x))) 6)
       (/ (* (- y x) x (dec (* 4 x x))) 3))))

(defn calc-rectangles
  [m n]
  (apply + (for [x (range 1 (inc m))
                 y (range 1 (inc n))]
             (+ (sub-rectangles x y)
                (sub-diagonal-rectangles x y)))))

(defn euler-147
  []
  (calc-rectangles 47 43))
