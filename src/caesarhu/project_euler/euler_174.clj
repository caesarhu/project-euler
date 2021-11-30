(ns caesarhu.project-euler.euler-174
  (:require [clojure.math.numeric-tower :as math]))

(defn root
  [limit x]
  (let [r (- (* x x) (inc limit))]
    (if (pos-int? r)
      (first (math/exact-integer-sqrt r))
      0)))

(defn brute-force
  [limit]
  (let [m (->> (apply merge-with + (for [i (range 3 (+ (quot limit 4) 2))
                                         :let [r (root limit i)]
                                         j (range (- i 2) r -2)]
                                     {(- (* i i) (* j j)) 1}))
               (map (fn [[k v]] {v 1}))
               (apply merge-with +))]
    (->> (range 1 11)
         (map m)
         (apply +))))

(comment
  (time (brute-force 1000000))
  )