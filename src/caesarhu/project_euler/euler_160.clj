(ns caesarhu.project-euler.euler-160
  (:require [clojure.math.numeric-tower :as math]
            [caesarhu.shun-tools.math-misc :as misc]))

(defn legendre
  [n p]
  (quot (- n (apply + (misc/digits n p)))
        (dec p)))

(defn base-5-remainder
  [n d]
  (let [p5 (math/expt 5 d)
        quot-seq (take-while pos-int? (iterate #(quot % 5) n))
        *mod-p5 (fn [x y] (mod (* x y) p5))]
    (->> (for [i quot-seq
               :let [sign (if (even? (quot i p5)) 1 -1)]]
           (cons sign (remove #(zero? (mod % 5)) (range 1 (inc (mod i p5))))))
         (apply concat)
         (reduce *mod-p5 1))))

(defn solve
  [n d]
  (let [[p2 p5 p10] (map #(math/expt % d) [2 5 10])
        i2 (misc/power-mod p2 -1 p5)
        power-of-5 (legendre n 5)
        i5 (misc/power-mod (misc/power-mod 2 power-of-5 p5) -1 p5)
        r5 (base-5-remainder n d)]
    (long (mod (* p2 i2 r5 i5) p10))))

(defn euler-160
  []
  (time (solve (math/expt 10 12) 5)))
