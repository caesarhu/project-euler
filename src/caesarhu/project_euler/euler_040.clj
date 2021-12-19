(ns caesarhu.project-euler.euler-040
  (:require [caesarhu.shun-tools.math-misc :as misc]
            [clojure.math.numeric-tower :as math]))

(def digit-seq
  (mapcat misc/digits (rest (range))))

(defn answer-seq
  [n]
  (->> (for [i (range n)]
         (math/expt 10 i))
       (map #(nth digit-seq (dec %)))))

(defn solve
  [n]
  (apply * (answer-seq n)))

(comment
  (answer-seq 7)
  (solve 7)
  )


