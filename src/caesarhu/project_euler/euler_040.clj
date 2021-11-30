(ns caesarhu.project-euler.euler-040
  (:require [caesarhu.shun-tools.math-misc :as misc]
            [clojure.math.numeric-tower :as math]))

(def digit-seq
  (mapcat misc/digits (rest (range))))

(defn solve
  [n]
  (->> (for [i (range n)]
         (math/expt 10 i))
       (map #(nth digit-seq (dec %)))
       (apply *)))

; (time (solve 7))


