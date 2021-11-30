(ns caesarhu.project-euler.euler-045
  (:require [caesarhu.shun-tools.math-misc :as misc]
            [clojure.math.numeric-tower :as math]))

(defn triple?
  [n]
  (and (misc/is-triangular? n)
       (misc/is-pentagonal? n)
       (misc/is-hexagonal? n)))

(defn solve
  []
  (->> (drop 286 (range))
       (map misc/triangle)
       (filter triple?)
       first))

(defn brute-force
  []
  (->> (drop 144 (range))
       (map misc/hexagon)
       (filter misc/is-pentagonal?)
       (filter misc/is-triangular?)
       first))

(comment
  (time (brute-force))
  (time (solve))
  )