(ns caesarhu.project-euler.euler-102
  (:require [clojure.math.numeric-tower :as math]
            [clojure.string :as str]))

(defn area
  [[x1 y1] [x2 y2] [x3 y3]]
  (math/abs (/ (+ (* x1 (- y2 y3)) (* x2 (- y3 y1)) (* x3 (- y1 y2))) 2)))

(defn origin-inside?
  [[x1 y1] [x2 y2] [x3 y3]]
  (let [a0 (area [x1 y1] [x2 y2] [x3 y3])
        a1 (area [0 0] [x2 y2] [x3 y3])
        a2 (area [x1 y1] [0 0] [x3 y3])
        a3 (area [x1 y1] [x2 y2] [0 0])]
    (< (math/abs (- a0 a1 a2 a3)) 1/1000000)))

(def triangles-file "resources/data/p102_triangles.txt")

(defn get-triangles
  []
  (->> (str/split-lines (slurp triangles-file))
       (map #(str/split % #","))
       (map (fn [coll] (map #(Long/parseLong %) coll)))
       (map (fn [coll] (partition 2 coll)))))

(defn euler-102
  []
  (->> (get-triangles)
       (map #(apply origin-inside? %))
       (filter true?)
       count))

(comment
  (euler-102)
  )