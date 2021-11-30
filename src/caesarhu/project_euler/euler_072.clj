(ns caesarhu.project-euler.euler-072
  (:require [caesarhu.shun-tools.primes :as p]))

(defn solve
  [limit]
  (->> (range 2 (inc limit))
       (map p/phi)
       (apply +)))
