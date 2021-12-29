(ns caesarhu.project-euler.euler-068
  (:require [clojure.math.combinatorics :as comb]))

(defn euler-68-magic [n]
  {:pre [(odd? n)]}
  (let [magic (/ (+ 1 2 (inc n) (* 2 n) n (dec n)) 2)
        outers (concat [(inc n)] (reverse (range (+ 2 n) (inc (* 2 n)))))
        inners (butlast (reductions #(- magic %1 %2) n outers))]
    (apply str (mapcat conj (partition 2 1 (cycle inners)) outers))))

(comment
  (time (euler-68-magic 5))
  )