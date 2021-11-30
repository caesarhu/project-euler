(ns caesarhu.project-euler.euler-189-2
  (:require [rolling-stones.core :as sat :refer [! at-least at-most exactly]]
            [caesarhu.shun-tools.or-tools :as or]))

(defn row-length
  [row]
  (inc (* row 2)))

(defn index->pos
  [x]
  (let [i (dec x)
        row (last (for [x (iterate inc 0)
                        :let [x2 (* x x)]
                        :while (<= x2 i)]
                    x))]
    [row (- i (* row row))]))

(defn pos->index
  [[row r]]
  (+ (* row row) r 1))

(defn pos-neighbours
  [height [row column :as pos]]
  (let [direction (if (even? column) 1 -1)
        relate-row (let [r (+ row direction)]
                     (if (< -1 r height) r nil))
        relate-row-pos (when relate-row
                         [relate-row (+ column direction)])]
    (->> ((juxt dec inc) column)
         (filter #(<= 0 % (dec (row-length row))))
         (map #(vector row %))
         (#(if relate-row-pos (conj % relate-row-pos) %)))))

(defn neighbours
  [height index]
  (->> (pos-neighbours height (index->pos index))
       (map pos->index)
       set))

(defn index->var-base
  [i]
  (inc (* 3 (dec i))))

(defn digits-cnf
  [height]
  (for [i (range 1 (inc (* height height)))]
    (exactly 1 (map #(+ (index->var-base i) %) (range 3)))))

(defn neighbours-cnf
  [height]
  (for [i (range 1 (inc (* height height)))
        :let [i-base (index->var-base i)]
        neighbour (neighbours height i)
        :let [neighbour-base (index->var-base neighbour)]
        j (range 3)]
    (at-most 1 [(+ i-base j) (+ neighbour-base j)])))

(defn triangle-cnf
  [height]
  (concat (digits-cnf height) (neighbours-cnf height)))

(defn solve
  [height]
  (count (sat/solutions (triangle-cnf height))))

(comment
  (time (solve 4))
  )