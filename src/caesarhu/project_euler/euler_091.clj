(ns caesarhu.project-euler.euler-091
  (:require [clojure.math.combinatorics :as comb]
            [clojure.math.numeric-tower :as math]))

(defn diff-square
  [[x1 y1] [x2 y2]]
  (let [xf (- x1 x2)
        yf (- y1 y2)]
    (+ (* xf xf) (* yf yf))))

(defn right-angle?
  [a b c]
  (->> (comb/combinations [a b c] 2)
       (map #(apply diff-square %))
       (sort)
       (#(= (last %) (apply + (drop-last %))))))

(defn euler-091
  [limit]
  (let [n-range (range (inc limit))
        point-range (rest (comb/cartesian-product n-range n-range))]
    (count (for [points-2 (comb/combinations point-range 2)
                 :when (apply right-angle? [0 0] points-2)]
             points-2))))

(defn euler-091-fast
  [limit]
  (let [c (* 3 (* limit limit))]
    (reduce + c (for [x1 (range 1 (inc limit))
                      y1 (range 1 (inc limit))
                      :let [p (math/gcd x1 y1)]]
                  (* 2 (min (quot (* p x1) y1) (quot (* p (- limit y1)) x1)))))))

