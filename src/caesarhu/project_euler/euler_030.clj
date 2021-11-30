(ns caesarhu.project-euler.euler-030
  (:require [clojure.math.numeric-tower :as math]
            [caesarhu.shun-tools.math-misc :as misc]))

(defn nth-power-sum
  [n power]
  (->> (misc/digits n)
       (map #(math/expt % power))
       (apply +)))

(defn nth-power-sum?
  [n power]
  (= n (nth-power-sum n power)))

(defn solve
  [power]
  (->> (range 2 (math/expt 10 (inc power)))
       (filter #(nth-power-sum? % power))
       (#(hash-map :sum (apply + %) :values %))))