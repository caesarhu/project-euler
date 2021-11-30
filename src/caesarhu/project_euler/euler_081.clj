(ns caesarhu.project-euler.euler-081
  (:require [clojure.string :as str]
            [caesarhu.shun-tools.a-star :refer [A*-search]]
            [loom.graph :as g]
            [loom.alg :as alg]))

(def test-data
  [[131,673,234,103,18]
   [201,96,342,965,150]
   [630,803,746,422,111]
   [537,699,497,121,956]
   [805,732,524,37,331]])

(defn get-data
  []
  (->> (slurp "resources/data/p081_matrix.txt")
       (str/split-lines)
       (map #(str/split % #","))
       (map (fn [coll]
              (map #(Long/parseLong %) coll)))
       (map vec)
       vec))

(defn solve
  [matrix]
  (let [mat   matrix
        m     (count mat)
        n     (count (first mat))
        cost  (fn [[i j]] (nth (nth mat (dec i)) (dec j)))
        start [1 1]
        goal  [m n]
        goal? (fn [e] (= e goal))
        est   (fn [[i j]] (+ (- m i) (- n j)))
        neigh (fn [[i j]]
                (merge
                  (when (< i m) {[(inc i) j] (cost [(inc i) j])})
                  (when (< j n) {[i (inc j)] (cost [i (inc j)])})))
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
                                  (when (< j (dec n)) [i (inc j)]))))
        add-weight (fn [[pos1 w1] [pos2 w2]]
                     [pos1 pos2  (+ w1 w2)])]
    (->> (for [i (range m)
               j (range n)
               :let [node-weight (weight [i j])
                     neigh-weight (map weight (neighbor [i j]))]]
           (map #(add-weight node-weight %) neigh-weight))
         (apply concat)
         (apply g/weighted-digraph))))

(defn euler-081
  [matrix]
  (let [edge (count matrix)
        weight-addition (+ (get-in matrix [0 0]) (get-in matrix [(dec edge) (dec edge)]))
        wdg (->weight-digraph matrix)]
    (-> (alg/dijkstra-path-dist wdg [0 0] [(dec edge) (dec edge)])
        last
        (+ weight-addition)
        (/ 2))))

(comment
  (->weight-digraph (get-data))
  (time (solve (get-data)))
  (time (euler-081 (get-data)))
  )