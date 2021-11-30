(ns caesarhu.project-euler.euler-120
  (:require [caesarhu.shun-tools.primes :as p]
            [clojure.math.numeric-tower :as math]))

(defn max-remainder
  [a]
  (- (* a a)
     (if (even? a)
       (+ a a)
       a)))

(defn euler-120
  [limit]
  (->> (range 3 (inc limit))
       (map max-remainder)
       (apply +)))
