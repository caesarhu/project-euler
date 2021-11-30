(ns caesarhu.project-euler.euler-046
  (:require [caesarhu.shun-tools.primes :as p]
            [clojure.math.numeric-tower :as math]))

(defn prime-and-twice-a-square?
  [n]
  (let [primes-seq (take-while #(< % n) p/primes)]
    (->> (for [p primes-seq
               :let [sqr (math/sqrt (- n p))]
               i (range 1 (inc sqr))
               :let [i2 (* i i 2)]
               :when (= n (+ p i2))]
           true)
         first
         boolean)))

(defn solve
  []
  (->> p/composites
       (filter odd?)
       (some #(and (not (prime-and-twice-a-square? %)) %))))

(comment
  (time (solve))
  )