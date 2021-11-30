(ns caesarhu.project-euler.euler-113
  (:require [caesarhu.shun-tools.math-misc :as misc]))

(defn next-creasing
  [[xs decreasing]]
  (let [new-xs (->> (map #(drop % xs) (range (count xs)))
                    (map #(apply + %)))]
    [new-xs (apply + decreasing new-xs)]))

(def next-creasing-seq
  (iterate next-creasing [(repeat 10 1) 10]))

(defn nth-creaing
  [n]
  (let [[xs decreasing] (nth next-creasing-seq (dec n))]
    (+ (apply + xs) decreasing (- (* 10 n)) (- 1))))