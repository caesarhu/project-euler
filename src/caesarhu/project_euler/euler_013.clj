(ns caesarhu.project-euler.euler-013
  (:require [clojure.string :as str]))

(def number-file "resources/data/50-digit-numbers.txt")

(defn get-numbers
  [fname]
  (->> (slurp fname)
       (str/split-lines)
       (map bigint)))

(defn solve
  []
  (->> (get-numbers number-file)
       (apply +)
       str
       (take 10)
       (apply str)))

; (time (solve))