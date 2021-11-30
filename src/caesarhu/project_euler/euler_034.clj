(ns caesarhu.project-euler.euler-034
  (:require [caesarhu.shun-tools.math-misc :as misc]))

(defn factorial-sum?
  [n]
  (let [factorial-sum (->> (misc/digits n)
                           (map #(nth misc/factorial-seq %))
                           (apply +))]
    (= n factorial-sum)))

(defn solve
  [n]
  (->> (range 3 n)
       (filter factorial-sum?)
       (apply +)))

(comment
  (time (solve 100000))
  )