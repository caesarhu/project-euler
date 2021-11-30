(ns caesarhu.project-euler.euler-052
  (:require [caesarhu.shun-tools.math-misc :as misc]))

(defn permuted-multiple?
  [n]
  (let [ds (sort (misc/digits n))]
    (when (apply distinct? ds)
      (->> (range 2 7)
           (map #(* n %))
           (map misc/digits)
           (map sort)
           (apply = ds)))))

(defn solve
  []
  (->> (iterate inc 100000)
       (some #(and (permuted-multiple? %) %))))

(comment
  (time (solve))
  )