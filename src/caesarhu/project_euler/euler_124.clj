(ns caesarhu.project-euler.euler-124
  (:require [caesarhu.shun-tools.primes :as p]))

(defn radical
  [n]
  [(apply * (distinct (p/prime-factors-of n))) n])

(defn euler-124
  [limit target]
  (->> (range (inc limit))
       (map radical)
       sort
       (#(nth % target))))

; (time (euler-124 100000 10000))