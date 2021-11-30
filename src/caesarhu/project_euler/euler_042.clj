(ns caesarhu.project-euler.euler-042
  (:require [caesarhu.shun-tools.math-misc :as misc]))

(def fname "resources/data/words.txt")

(defn get-data [fname]
  (sort (clojure.string/split (slurp fname) #",")))

(defn word-score [word]
  (reduce + (map #(- (int %) 64) (filter #(not= % \") word))))

(defn solve
  []
  (->> (get-data fname)
       (map word-score)
       (filter misc/is-triangular?)
       count))