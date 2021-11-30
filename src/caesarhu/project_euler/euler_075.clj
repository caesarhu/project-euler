(ns caesarhu.project-euler.euler-075
  (:require [caesarhu.shun-tools.primes :as p]
            [clojure.math.numeric-tower :as math]))

(defn pythagorean-perimeters [limit]
  (let [sq (int (Math/sqrt limit))]
    (frequencies
      (for [n (range 1 sq 2)
            m (range 2 sq 2)
            :when (p/coprime? m n)       ;; Avoid duplicates
            p [(+ (math/abs (- (* m m) (* n n))) (* 2 m n) (* m m) (* n n))]
            k (range 1 (/ (inc limit) p))] ;; scaled triangles also give perims
        (* p k)))))

(defn solve [n]
  (count (filter #(= 1 (second %)) (pythagorean-perimeters n))))

; (time (solve 1500000))