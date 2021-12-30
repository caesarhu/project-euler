(ns caesarhu.project-euler.euler-046
  (:require [caesarhu.primes :as p]
            [clojure.math.numeric-tower :as math]))

(defn is-conjecture?
  [n]
  (let [sqr (math/sqrt n)]
    (some p/is-prime? (for [i (rest (range))
                            :let [i2 (* i i 2)]
                            :while (> n i2)]
                        (- n i2)))))

(defn solve
  []
  (->> (iterate #(+ % 2) 3)
       (remove p/is-prime?)
       (filter (complement is-conjecture?))
       first))

(comment
  (time (solve))
  )