(ns caesarhu.project-euler.euler-169
  (:require [clojure.math.numeric-tower :as math]
            [clojure.core.memoize :as m]))

(def fusc
  (m/lu
   (fn [n]
     (cond
       (< n 2) n
       (even? n) (fusc (quot n 2))
       :else (+ (fusc (quot (dec n) 2)) (fusc (quot (inc n) 2)))))
   :lu/threshold (math/expt 10 3)))

(defn stern
  [n]
  (loop [n n
         [p q] [1 0]]
    (if (zero? n)
      q
      (recur (quot n 2) (if (even? n) [(+ p q) q] [p (+ p q)])))))

(defn solve
  [n]
  (stern (inc n)))

(comment
  (time (solve (math/expt 10 25)))
  )