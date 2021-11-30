(ns caesarhu.project-euler.euler-151
  (:require [clojure.math.numeric-tower :as math]))

(defn cut-envelope
  [coll i]
  (let [ix (coll i)
        [head tail] (split-at i coll)]
    (when (pos-int? ix)
      (vec (concat head [(dec ix)] (map inc (rest tail)))))))

(defn next-state
  [m]
  (apply (partial merge-with +) (for [[coll probability] m
                                      i (range (count coll))
                                      :let [ix (coll i)]
                                      :when (pos-int? ix)]
                                  {(cut-envelope coll i)
                                   (* probability (/ ix (apply + coll)))})))

(defn only-one?
  [[coll probability]]
  (and (= 1 (apply + coll))
       (not (== 1 probability))))

(defn round6
  [n]
  (-> (math/round (* n 1000000))
      (/ 1000000)
      double))

(defn euler-151
  []
  (-> (iterate (fn [[acc state]]
                 (let [new-state (next-state state)
                       only-one (->> (filter only-one? new-state)
                                     (map last)
                                     (apply +))]
                   [(+ acc only-one) new-state]))
               [0 {[1 1 1 1] 1}])
      (nth 15)
      first
      round6))

; (time (euler-151))