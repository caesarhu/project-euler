(ns caesarhu.project-euler.euler-148-old
  (:require [caesarhu.shun-tools.math-misc :as misc]
            [clojure.math.numeric-tower :as math]
            [caesarhu.shun-tools.primes :as p]))

; base on https://projecteuler.net/thread=148 hyperdex's solution

(defn brute-force
  [n]
  (loop [x n
         p28 1
         result 0]
    (if (<= x 0)
      result
      (let [d (mod x 7)]
        (recur (quot x 7) (*' p28 28) (quot (*' (inc d) (+' (*' d p28) (*' 2 result))) 2))))))

(defn euler-148
  []
  (brute-force 1000000000))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn factorial
  [n]
  (loop [cnt n
         acc 1]
    (if (< cnt 2)
      acc
      (recur (dec cnt) (*' acc cnt)))))

(defn coefficients
  [n i]
  (quot (factorial n)
        (* (factorial i) (factorial (- n i)))))

(defn pascal-triangle
  [n]
  (for [i (range (inc n))]
    (coefficients n i)))

(defn pascal-coprime
  [n p]
  (reduce * (map inc (misc/digits n p))))

(defn coprime-sum
  [n p]
  (reduce + (map #(pascal-coprime % p) (range n))))

(defn euler-148
  [limit prime]
  (let [prime-base (quot (* prime (inc prime)) 2)
        digits (vec (misc/digits limit prime))
        calc (fn [header d k] (* header (quot (* d (inc d)) 2) (math/expt prime-base k)))]
    (loop [header 1
           digits digits
           result 0]
      (if (empty? digits)
        result
        (let [d (first digits)
              k (dec (count digits))]
          (recur (* header (inc d)) (rest digits) (+ result (calc header d k))))))))

; (time (euler-148 (math/expt 10 9) 7))
