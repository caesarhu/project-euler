(ns caesarhu.project-euler.euler-007
  (:require [caesarhu.shun-tools.primes :refer [primes]]))

(defn solve
  [n]
  (nth primes (dec n)))

(comment
  (time (solve 10001))
  )

