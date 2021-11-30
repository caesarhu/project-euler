(ns caesarhu.project-euler.euler-184
  (:require [clojure.math.numeric-tower :as math]
            [clojure.math.combinatorics :as combo]))

(defn area
  [[x1 y1] [x2 y2] [x3 y3]]
  (math/abs (/ (+ (* x1 (- y2 y3)) (* x2 (- y3 y1)) (* x3 (- y1 y2))) 2)))

(defn origin-inside?
  [[x1 y1] [x2 y2] [x3 y3]]
  (let [a0 (area [x1 y1] [x2 y2] [x3 y3])
        a1 (area [0 0] [x2 y2] [x3 y3])
        a2 (area [x1 y1] [0 0] [x3 y3])
        a3 (area [x1 y1] [x2 y2] [0 0])
        result (math/abs (- a0 a1 a2 a3))]
    (< result 1/1000000)))

(defn square
  [n]
  (* n n))

(defn vertices
  [r]
  (for [i (range (- r) r)
        j (range (- r) r)
        :when (and (not= [0 0] [i j])
                   (< (+ (square i) (square j)) (square r)))]
    [i j]))

(comment
  (origin-inside? [-1 1] [1 1] [0 -1])
  (vertices 3)
  (->> (vertices 2)
       (#(combo/combinations % 3))
       (filter #(apply origin-inside? %)))
  )