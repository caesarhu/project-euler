(ns caesarhu.project-euler.euler-025
  (:require [caesarhu.shun-tools.math-misc :as misc]
            [clojure.math.numeric-tower :as math]))

(def fib-seq
  ((fn fib [a b]
     (lazy-seq (cons a (fib b (+' a b)))))
   0 1))

(def fib-digits
  (map #(vector %1 (count (misc/digits %2))) (range) fib-seq))

(defn brute-force
  [n]
  (some (fn [[idx digits]]
          (and (= digits n) idx))
        fib-digits))

; https://projecteuler.net/thread=25;page=8
; CBurkhart's solution

(defn solve
  [digits]
  (let [n (+ (Math/log10 (math/sqrt 5))
             (dec digits))
        d (Math/log10 (/ (inc (math/sqrt 5)) 2))]
    (int (math/ceil (/ n d)))))

(comment
  (time (brute-force 1000))
  (time (solve 1000))
  )