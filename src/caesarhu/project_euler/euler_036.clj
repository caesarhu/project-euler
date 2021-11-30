(ns caesarhu.project-euler.euler-036
  (:require [caesarhu.shun-tools.math-misc :as misc]))

(defn palindrome?
  [coll]
  (= coll (reverse coll)))

(defn both-palindrome?
  [n]
  (and (palindrome? (misc/digits n))
       (palindrome? (misc/digits n 2))))

(defn solve
  [n]
  (->> (range 1 n)
       (filter both-palindrome?)
       (apply +)))

(comment
  (time (solve 1000000))
  )