(ns caesarhu.project-euler.euler-061
  (:require [caesarhu.shun-tools.math-misc :as misc]
            [clojure.math.combinatorics :as comb]))

(defn limit
  ([f] (limit f [1000 10000]))
  ([f [start end]]
   (->> (misc/number-seq f)
        (drop-while #(< % start))
        (take-while #(< % end)))))

(defn next-range [n]
  (let [hundreds (* 100 (rem n 100))]
    [hundreds (+ hundreds 100)]))

(defn cycles [fs]
  (letfn [(filter-pred [xs]
            (and
              (= (count xs) (inc (count fs)))
              (= (first xs) (last xs))
              (every? #(>= % 1000) xs)))
          (cycles0 [fs limits x]
            (if (nil? fs)
              x
              (cons x
                    (for [n (limit (first fs) limits)]
                      (cycles0 (next fs) (next-range n) n)))))]
    (->> (cycles0 (concat fs [(first fs)]) [1000 10000] nil)
         (map flatten)
         (filter filter-pred)
         first
         distinct)))

(defn solve []
  (->> (comb/permutations (list misc/triangle misc/square misc/pentagon misc/hexagon misc/heptagon misc/octagon))
       (map cycles)
       (remove empty?)
       (map (partial reduce +))
       first))