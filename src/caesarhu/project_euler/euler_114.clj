(ns caesarhu.project-euler.euler-114
  (:require [clojure.math.combinatorics :as comb]))

(defn counting-block
  [n]
  (let [adj (concat [0 0 0 0] (cycle [1 1 0 -1 -1 0]))]
    (loop [counter 2
           result [0 1 1]]
      (tap> result)
      (cond
        (= n counter) (last result)
        (< n 3) (nth result n)
        :else (recur (inc counter) (conj result (apply + (nth adj (inc counter))
                                                       (take-last 2 result))))))))

(defn euler-114
  []
  (counting-block 50))
