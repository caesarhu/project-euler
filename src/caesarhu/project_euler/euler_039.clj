(ns caesarhu.project-euler.euler-039
  (:require [clojure.math.numeric-tower :as math]
            [criterium.core :refer [bench]]))

(defn triangles
  [p] ;; return only right angle triangles
  (for [b (range 1 (quot p 2))
        a (range 1 b)
        :let [c (- p a b)]
        :when (= (* c c) (+ (* a a) (* b b)))]
    [a b c]))

(defn brute-force
  [limit]
  (apply max-key #(count (triangles %)) (range 1 (inc limit))))

(defn coprime?
  [a b]
  (= 1 (math/gcd a b)))

(defn solve
  [limit]
  (->> (for [m (range 2 (math/sqrt (/ limit 2)))
             n (range (if (even? m) 1 2) m 2) :when (coprime? m n)
             :let [p (* 2 m (+ m n))] :when (<= p limit)
             i (range 1 (/ limit p))]
         (* p i))
       frequencies
       (apply max-key val)
       first))

(comment
  (time (solve 1000))
  (time (brute-force 1000))
  )