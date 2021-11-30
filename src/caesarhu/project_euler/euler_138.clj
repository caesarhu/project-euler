(ns caesarhu.project-euler.euler-138
  (:require [clojure.math.numeric-tower :as math]
            [caesarhu.shun-tools.pell-equation :as pell]))

(defn generate-triangle
  [limit]
  (loop [counter 0
         x 0, y -1
         result []]
    (if (= limit counter)
      result
      (let [x-new (+ (* -9 x) (* -4 y) 4)
            y-new (+ (* -20 x) (* -9 y) 8)]
        (recur (inc counter) x-new y-new (conj result [x-new y-new]))))))

(defn euler-138
  [limit]
  (->> (generate-triangle limit)
       (map last)
       (map math/abs)
       (apply +)))

(defn euler-138-pell
  [limit]
  (->> (rest (pell/pell-solutions 5 -1))
       (filter #(odd? (last %)))
       (take limit)
       (map last)
       (apply +)))