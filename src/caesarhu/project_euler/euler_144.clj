(ns caesarhu.project-euler.euler-144
  (:require [clojure.math.numeric-tower :as math]))

(defn next-point
  [point-0 point-1]
  (let [[x0 y0] point-0
        [x1 y1] point-1
        m (/ (- y0 y1) (- x0 x1))
        normal (/ y1 (* 4 x1))
        n (/ (- (* 2 normal) (* m (- 1 (* normal normal)))) (+ 1 (- (* normal normal)) (* 2 normal m)))
        x2 (/ (- (* n n x1) (* 4 x1) (* 2 n y1)) (+ 4 (* n n)))
        y2 (+ y1 (* n (- x2 x1)))]
    [point-1 [x2 y2]]))

(defn euler-144
  [point-0 point-1]
  (loop [p0 point-0, p1 point-1
         result 0]
    (let [[x1 y1] p1]
      (if (and (<= (math/abs x1) 0.01)
               (> y1 0))
        result
        (let [[t0 t1] (next-point p0 p1)]
          (recur t0 t1 (inc result)))))))

; (time (euler-144 [0.0 10.1] [1.4 -9.6]))