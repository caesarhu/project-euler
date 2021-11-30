(ns caesarhu.project-euler.euler-067
  (:require [caesarhu.shun-tools.a-star :refer [A*-search]]))

(def fname "resources/data/p067_triangle.txt")

(defn get-triangle []
  (letfn [(parse-nums [s] (map #(Integer/parseInt %) (clojure.string/split s #"\W")))]
    (->> (map parse-nums (clojure.string/split-lines (slurp fname)))
         (map vec)
         vec)))

(defn shorten-level
  [f level-1 level-2]
  (->> (partition 2 1 level-1)
       (map (fn [a [b c]]
              (apply f [(+ a b) (+ a c)])) level-2)))

(defn f-path
  [f coll]
  (loop [coll (reverse coll)]
    (if (= 1 (count coll))
      (ffirst coll)
      (recur (cons (shorten-level f (first coll) (second coll)) (drop 2 coll))))))

(defn solve
  []
  (f-path max (get-triangle)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn merge-rows[a b]
  (map + (map #(apply max %) (partition 2 1 a)) b))

(defn euler-67
  []
  (reduce merge-rows (reverse (get-triangle))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; use A*-search
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn euler-67-A*
  [matrix]
  (let [m (count matrix)
        cost (fn [[i j]] (get-in matrix [(dec i) (dec j)]))
        minus-cost (fn [[i j]] (- 100 (cost [i j])))
        start [1 1]
        goal? (fn [[i _]] (= i m))
        est (fn [[i _]] (- m i))
        neigh (fn [[i j]]
                (when (< i m) {[(inc i) j]       (minus-cost [(inc i) j])
                               [(inc i) (inc j)] (minus-cost [(inc i) (inc j)])}))
        path (A*-search est neigh start goal?)]
    (reduce + (map cost path))))

(comment
  (time (solve))
  (time (euler-67))
  (time (euler-67-A* (get-triangle)))
  )