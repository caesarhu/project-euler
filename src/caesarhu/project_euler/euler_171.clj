(ns caesarhu.project-euler.euler-171
  (:require [caesarhu.shun-tools.math-misc :as misc]
            [clojure.math.numeric-tower :as math]))

(defn square
  [n]
  (* n n))

(defn generate-sum
  [limit]
  (loop [num-map (into {} (map #(vector (square %) 1) (range 10)))
         sum-map (into {} (map #(vector (square %) %) (range 10)))
         i 1]
    (if (= i limit)
      sum-map
      (let [new-num-map (reduce #(merge-with +' %1 %2)
                                {}
                                (for [x (range 10)
                                      [k v] num-map]
                                  {(+' (square x) k) v}))
            new-sum-map (reduce #(merge-with +' %1 %2)
                                {}
                                (for [x (range 10)
                                      [k v] sum-map]
                                  {(+' (square x) k) (+' (*' (math/expt 10 i) x (num-map k)) v)}))]
        (recur new-num-map new-sum-map (inc i))))))

(defn solve
  [limit]
  (let [sum-map (generate-sum limit)]
    (->> (filter #(misc/is-square? (key %)) sum-map)
         vals
         (apply +')
         (#(mod % (math/expt 10 9)))
         int)))

(comment
  (time (solve 20))
  (count (generate-sum 20))
  )
