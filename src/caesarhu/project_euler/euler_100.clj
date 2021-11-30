(ns caesarhu.project-euler.euler-100
  (:require [clojure.math.numeric-tower :as math]
            [caesarhu.shun-tools.pell-equation :refer [pell-solutions]]))

(defn next-b [b n] (+ (* 3 b) (* 2 n) -2))
(defn next-n [b n] (+ (* 4 b) (* 3 n) -3))

(defn solve
  [target]
  (loop [b 15, n 21]
    (if (< n target)
      (recur (next-b b n) (next-n b n))
      b)))

; https://projecteuler.net/thread=100;page=8
; sorrowise's solution

(defn solve-pell
  [limit]
  (let [->x (fn [n] (quot (inc n) 2))]
    (->> (pell-solutions 2 -1)
         (map #(map ->x %))
         (drop-while #(> limit (first %)))
         first
         last)))

(comment
  (time (solve-pell (math/expt 10 12)))
  (time (solve (math/expt 10 12)))
  )