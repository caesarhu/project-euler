(ns caesarhu.project-euler.euler-105
  (:require [clojure.math.combinatorics :as comb]
            [injest.core :as injest :refer [x> x>> => =>>]]
            [clojure.string :as str]))

(defn rule-bigger?
  [coll]
  (let [sorted (sort coll)
        length (count coll)]
    (every? true? (for [i (range 2 length)
                        :let [j (dec i)
                              sum-i (apply + (take i sorted))
                              sum-j (apply + (take-last j sorted))]]
                    (> sum-i sum-j)))))

(defn rule-sum?
  [coll]
  (->> (comb/subsets coll)
       (map #(apply + %))
       (apply distinct?)))

(defn special-sum-set?
  [coll]
  (and (rule-bigger? coll) (rule-sum? coll)))

(defn euler-105
  []
  (x>> (str/split-lines (slurp "resources/data/p105_sets.txt"))
       (map #(str/split % #","))
       (map (fn [coll]
              (map #(Long/parseLong %) coll)))
       (filter special-sum-set?)
       (map #(apply + %))
       (apply +)))

(comment
  (time (euler-105))
  )