(ns caesarhu.project-euler.euler-083
  (:require [clojure.string :as str]
            [caesarhu.shun-tools.a-star :refer [A*-search]]
            [loom.graph :as g]
            [loom.alg :as alg]))

(def fname "resources/data/p083_matrix.txt")

(defn get-data
  [fname]
  (->> (slurp fname)
       (str/split-lines)
       (map #(str/split % #","))
       (map (fn [coll]
              (map #(Long/parseLong %) coll)))
       (map vec)
       vec))

(defn euler-83
  [matrix]
  (let [mat   matrix
        m     (count mat)
        n     (count (first mat))
        cost  (fn [[i j]] (nth (nth mat (dec i)) (dec j)))
        start [1 1]
        goal  [m n]
        goal? (fn [e] (= e goal))
        est   (fn [[i j]] 0)
        neigh (fn [[i j]]
                (merge
                  (when (< i m) {[(inc i) j] (cost [(inc i) j])})
                  (when (< j n) {[i (inc j)] (cost [i (inc j)])})
                  (when (< 1 i) {[(dec i) j] (cost [(dec i) j])})
                  (when (< 1 j) {[i (dec j)] (cost [i (dec j)])})))
        path  (A*-search est neigh start goal?)]
    (reduce + (map cost path))))

(defn ->weight-digraph
  [matrix]
  (let [m     (count matrix)
        n     (count (first matrix))
        weight (fn [[i j]]
                 [[i j] (get-in matrix [i j])])
        neighbor (fn [[i j]]
                   (remove nil? (list
                                  (when (< i (dec m)) [(inc i) j])
                                  (when (< j (dec n)) [i (inc j)])
                                  (when (> i 0) [(dec i) j])
                                  (when (> j 0) [i (dec j)]))))
        add-weight (fn [[pos1 w1] [pos2 w2]]
                     [pos1 pos2  (+ w1 w2)])]
    (->> (for [i (range m)
               j (range n)
               :let [node-weight (weight [i j])
                     neigh-weight (map weight (neighbor [i j]))]]
           (map #(add-weight node-weight %) neigh-weight))
         (apply concat)
         (apply g/weighted-digraph))))

(defn euler-083-loom
  [matrix]
  (let [edge (count matrix)
        weight-addition (+ (get-in matrix [0 0]) (get-in matrix [(dec edge) (dec edge)]))
        wdg (->weight-digraph matrix)]
    (-> (alg/dijkstra-path-dist wdg [0 0] [(dec edge) (dec edge)])
        last
        (+ weight-addition)
        (/ 2))))

(comment
  (time (euler-083-loom (get-data fname)))
  (time (euler-83 (get-data fname)))
  )