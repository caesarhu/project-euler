(ns caesarhu.project-euler.euler-093
  (:require [clojure.math.combinatorics :as comb]
            [clojure.set :as set]
            [criterium.core :refer [bench quick-bench]]
            [net.cgrand.xforms :as x]))

(defn count-consecutive
  [ns]
  (let [xform (comp (filter pos?)
                    (filter integer?)
                    (x/sort)
                    (x/partition 2 1)
                    (take-while (fn [[a b]] (= a (dec b)))))]
    (inc (count (sequence xform ns)))))

(defn partition-sub
  [coll]
  (map #(vector %1 %2) (reverse coll) (comb/combinations coll (dec (count coll)))))

(defn compute-all
  [coll]
  (if (= 1 (count coll))
    [(first coll) (- (first coll))]
    (apply set/union (for [[m coll] (partition-sub coll)
                           n [m (- m)]
                           other (compute-all coll)
                           :let [r0 [(+ n other) (- n other) (* n other)]]]
                       (if (zero? other)
                         (set r0)
                         (set (conj r0 (/ n other))))))))

(defn count-arithmetic
  [coll]
  (count-consecutive (compute-all coll)))

(defn euler-093
  []
  (->> (comb/permuted-combinations (range 1 10) 4)
       (apply max-key #(count-arithmetic %))
       sort
       (apply str)))
