(ns caesarhu.project-euler.euler-056
  (:require [caesarhu.shun-tools.math-misc :as misc]
            [clojure.math.numeric-tower :as math]))

(defn digital-sum
  [n]
  (apply + (misc/digits n)))

(defn solve
  [n]
  (->> (for [a (range 1 n)
             b (range 1 n)]
         (math/expt a b))
       (map digital-sum)
       (apply max)))

(comment
  (time (solve 100))
  )