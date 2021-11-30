(ns caesarhu.project-euler.euler-082
  (:require [clojure.string :as str]
            [caesarhu.shun-tools.a-star :refer [A*-search]]
            [loom.graph :as g]
            [loom.alg :as alg]))

(def test-data
  [[131  673 234 103 18]
   [201 96 342 965  150]
   [630  803  746  422  111]
   [537  699  497  121  956]
   [805  732  524   37  331]])

(defn get-data
  []
  (->> (slurp "resources/data/p082_matrix.txt")
       (str/split-lines)
       (map #(str/split % #","))
       (map (fn [coll]
              (map #(Long/parseLong %) coll)))
       (map vec)
       vec))

(defn solve-A*
  [matrix]
  (let [mat   matrix
        m     (count mat)
        n     (count (first mat))
        start [0 0] ;; special node left of the whole matrix
        goal? (fn [e] (= (second e) n))
        cost  (fn [[i j]]
                (if (= [i j] [0 0])
                  0
                  (nth (nth mat (dec i)) (dec j))))
        est   (fn [[i j]] (+ (- m i) (- n j)))
        neigh (fn [[i j]]
                (if (= [i j] [0 0])
                  (map (fn [i] [[i 1] (cost [i 1])]) (range 1 (inc m)))
                  (merge
                   (when (< i m) {[(inc i) j] (cost [(inc i) j])})
                   (when (< j n) {[i (inc j)] (cost [i (inc j)])})
                   (when (< 1 i) {[(dec i) j] (cost [(dec i) j])}))))
        path  (A*-search est neigh start goal?)]
    (reduce + (map cost path))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; try loom
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn ->wdg
  [matrix]
  (let [m     (count matrix)
        n     (count (first matrix))
        start-edges (for [i (range m)]
                      [:start [i 0] (get-in matrix [i 0])])
        end-edges (for [i (range n)]
                    [[i (dec n)] :end (get-in matrix [i (dec n)])])
        weight (fn [[i j]]
                 [[i j] (get-in matrix [i j])])
        neighbor (fn [[i j]]
                   (remove nil? (list
                                 (when (< i (dec m)) [(inc i) j])
                                 (when (> i 0) [(dec i) j])
                                 (when (< j (dec n)) [i (inc j)]))))
        add-weight (fn [[pos1 w1] [pos2 w2]]
                     [pos1 pos2  (+ w1 w2)])]
    (->> (for [i (range m)
               j (range n)
               :let [node-weight (weight [i j])
                     neigh-weight (map weight (neighbor [i j]))]]
           (map #(add-weight node-weight %) neigh-weight))
         (apply concat start-edges end-edges)
         (apply g/weighted-digraph))))

(defn solve-loom
  [matrix]
  (let [wdg (->wdg matrix)]
    (-> (alg/dijkstra-path-dist wdg :start :end)
        last
        (/ 2))))

(comment
  (time (solve-loom (get-data)))
  (time (solve-A* (get-data)))
  )