(ns caesarhu.project-euler.euler-125
  (:require [clojure.math.numeric-tower :as math]))

(defn palindromic?
  [n]
  (let [s (str n)]
    (= s (apply str (reverse s)))))

(defn square-sums
  [limit]
  (loop [squares (map #(* % %) (rest (range)))
         result #{}]
    (if (>= (apply + (take 2 squares)) limit)
      result
      (recur (rest squares) (into result (->> (rest (reductions + squares))
                                              (take-while #(< % limit))
                                              (filter palindromic?)))))))

(defn euler-125
  [limit]
  (apply + (square-sums limit)))

; (time (euler-125 100000000))
