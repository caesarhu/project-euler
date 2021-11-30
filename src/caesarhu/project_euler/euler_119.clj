(ns caesarhu.project-euler.euler-119
  (:require [clojure.math.numeric-tower :as math]
            [caesarhu.shun-tools.math-misc :as misc]))

(def limit 1000)

(defn x-power-number
  [n x]
  (= n (math/expt (apply + (misc/digits n)) x)))

(defn x-power-seq
  [x]
  (->> (range 2 limit)
       (map #(math/expt % x))
       (filter #(x-power-number % x))))

(defn digit-power-sum
  [max-power]
  (->> (range 2 (inc max-power))
       (mapcat x-power-seq)
       sort))

(defn euler-119
  [n]
  (nth (digit-power-sum 20) (dec n)))