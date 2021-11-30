(ns caesarhu.project-euler.euler-153
  (:require [clojure.math.numeric-tower :as math]
            [caesarhu.shun-tools.primes :as p]))

; base on vikt's solution https://projecteuler.net/thread=153;page=6#last

(defn square
  [n]
  (*' n n))

(defn sigma-nature
  [n]
  (let [[sq _] (math/exact-integer-sqrt n)]
    (- (apply + (for [x (range 1 (inc sq))
                      :let [y (quot n x)]]
                  (+ (* x y)
                     (/ (* y (inc y)) 2))))
       (/ (* sq sq (inc sq)) 2))))

(defn gaussian-sum
  [limit]
  (let [[sq _] (math/exact-integer-sqrt limit)]
    (+ (sigma-nature limit)
       (apply + (for [x (range 2 (inc sq))
                      y (range 1 (inc x))
                      :when (p/coprime? x y)
                      :let [sigma-arg (quot limit (+ (square x) (square y)))]]
                  (* 2 (+ x y) (sigma-nature sigma-arg))))
       (* 2 (sigma-nature (quot limit 2))))))

(defn euler-153
  []
  (gaussian-sum (math/expt 10 8)))

; (time (euler-153))
