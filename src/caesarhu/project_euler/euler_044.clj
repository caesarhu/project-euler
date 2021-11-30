(ns caesarhu.project-euler.euler-044
  (:require [criterium.core :refer [bench]]
            [clojure.math.numeric-tower :as math]))

(defn pentagon [n] (quot (* n (dec (* 3 n))) 2))

(defn is-pentagonal? 
  [x]
  (let [[q r] (math/exact-integer-sqrt (inc (* 24 x)))]
    (and (zero? r)
         (zero? (mod (inc q) 6)))))

(defn pentagonal-pair?
  [p1 p2]
  (and (is-pentagonal? (+ p1 p2))
       (is-pentagonal? (+ p1 p1 p2))))

(defn solve
  []
  (loop [from-seq (map pentagon (iterate inc 2))
         ps [(pentagon 1)]]
    (let [p (first from-seq)]
      (if-let [result (some #(and (pentagonal-pair? % p) p) ps)]
        result
        (recur (rest from-seq) (conj ps p))))))

(comment
  (time (solve))
  )
