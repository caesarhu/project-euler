(ns caesarhu.project-euler.euler-145
  (:require [caesarhu.shun-tools.math-misc :as misc]
            [clojure.math.numeric-tower :as math]))

(defn is-reverse?
  [n]
  (let [digits (misc/digits n)]
    (if (zero? (last digits))
      false
      (let [n-rev (misc/to-number (reverse digits))
            result (+ n n-rev)]
        (every? odd? (misc/digits result))))))

(defn euler-145
  [power] ; 10 power
  (for [i (range 1 (inc power))]
    (cond
      (< i 2) 0
      (even? i) (* 20 (math/expt 30 (quot (- i 2) 2)))
      (= 3 (mod i 4)) (* 20 5 (math/expt (* 20 25) (quot (- i 3) 4)))
      :else 0)))
