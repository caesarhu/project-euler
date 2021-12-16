(ns caesarhu.project-euler.euler-022
  (:require [clojure.string :as str]))

(def euler-022-file "resources/data/p022_names.txt")

(defn get-data [fname]
  (sort (str/split (slurp fname) #",")))

(defn name-score
  [names]
  (let [score (for [name names]
                (->> (seq name)
                     (map int)
                     (map #(- % 64))
                     (remove neg?)
                     (apply +)))
        counter (rest (range))]
    (->> (map #(* %1 %2) score counter)
         (apply +))))

(defn solve
  []
  (name-score (get-data euler-022-file)))

(comment
  (time (solve))
  )