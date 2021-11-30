(ns caesarhu.project-euler.euler-062
  (:require [caesarhu.shun-tools.math-misc :as misc]))

(defn cube
  [n]
  (* n n n))

(defn cube-map
  [limit]
  (->> (range 1 limit)
       (map #(hash-map (frequencies (misc/digits (cube %))) [%]))
       (apply merge-with concat)))

(defn solve
  [n limit]
  (->> (cube-map limit)
       (some #(and (>= (count (val %)) n)
                   (val %)))
       (#(hash-map :sum (cube (first %)) :values %))))