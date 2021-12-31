(ns caesarhu.project-euler.euler-048
  (:require [clojure.math.numeric-tower :as math]
            [caesarhu.shun-tools.math-misc :as misc]))

(defn solve
  [n]
  (->> (range 1N (inc n))
       (map #(math/expt % %))
       (apply +)
       (str)
       (take-last 10)
       (apply str)))

(defn solve-048
  [n]
  (let [digits (math/expt 10 10)]
    (reduce (fn [acc n]
              (mod (+ acc (misc/power-mod n n digits)) digits))
            0 (range 1 (inc n)))))

(comment
  (time (solve-048 1000))
  (time (solve 1000))
  )

