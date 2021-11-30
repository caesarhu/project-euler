(ns caesarhu.project-euler.euler-122
  (:require [clojure.set :refer [map-invert]]
            [clojure.math.combinatorics :as comb]))

; https://www.mathblog.dk/project-euler-122-efficient-exponentiation/

(defn back-track
  [limit]
  (let [cost (atom {})
        path (atom {})]
    (letfn [(recursive
             [power depth]
             (when (and (<= power limit)
                        (or (nil? (@cost power)) (<= depth (@cost power))))
               (swap! cost merge {power depth})
               (swap! path merge {depth power})
               (doseq [d (range depth -1 -1)]
                 (recursive (+ power (@path d)) (inc depth)))
               @cost))]
      (recursive 1 0))))

(defn solve
  [limit]
  (->> (back-track limit)
       vals
       (apply +)))

(comment
  (time (solve 200))
  )