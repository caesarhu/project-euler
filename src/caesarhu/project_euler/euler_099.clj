(ns caesarhu.project-euler.euler-099
  (:require [clojure.string :as str]))

(def base-exp "resources/data/p099_base_exp.txt")

(defn get-numbers
  [fname]
  (->> (str/split-lines (slurp fname))
       (map #(str/split % #","))
       (map (fn [coll]
              (map #(Long/parseLong %) coll)))))

(defn log-vals
  [coll]
  (->> (map (fn [[b e]] (* e (Math/log10 b))) coll)
       (map-indexed vector)))

(defn solve []
  (+ 1 (first (apply max-key
                     (fn [[i v]] v) (log-vals (get-numbers base-exp)))))) ; +1, since log-vals index is zero-based