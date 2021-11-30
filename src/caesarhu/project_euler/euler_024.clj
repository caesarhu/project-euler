(ns caesarhu.project-euler.euler-024
  (:require [clojure.math.combinatorics :as comb]))

(defn solve
  [n]
  (->> (comb/nth-permutation (range 10) n)
       (apply str)))

; (time (solve 1000000))