(ns caesarhu.project-euler.euler-079
  (:require [clojure.string :as str]))

(defn get-data [fname]
  (map #(map misc/char-to-int (seq %))
       (str/split-lines (slurp fname))))

(defn conj-in
  "Conjs v into a set under key k of map m."
  [m k v]
  (assoc m k (conj (or (m k) #{}) v)))

(defn solve []
  (->> (get-data "resources/data/p079_keylog.txt")
       (reduce (fn [m [a b c]] (-> (conj-in m a b)
                                   (conj-in a c)
                                   (conj-in b c))) {})
       (sort-by #(count (val %)))
       (reduce (fn [l [k vs]]
                 (apply concat [k] (apply disj vs l) [l])) nil)
       (apply str)))

; (time (solve "resources/data/p079_keylog.txt"))