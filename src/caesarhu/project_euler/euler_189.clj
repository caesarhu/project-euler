(ns caesarhu.project-euler.euler-189
  (:require [clojure.math.combinatorics :as combo]
            [caesarhu.shun-tools.primes :as p]))

(def colors #{0 1 2}) ; 0 :red 1 :green 2 :blue

(defn exclude
  [& more]
  (apply disj colors more))

(defn flip
  "row {[0] 1 [1] 1 [2] 1}
   {[0 0] 4, [0 1] 2, [0 2] 2, [1 0] 2, [1 1] 4, [1 2] 2, [2 0] 2, [2 1] 2, [2 2] 4}"
  [row]
  (->> (for [[k _] row
             :let [length (count k)]
             target (->> (map exclude k)
                         (apply combo/cartesian-product)
                         (map vec))]
         {target (row k)})
       (apply merge-with +)))

(defn row-extend
  [row]
  (->> (for [[k v] row
             color colors
             :let [new-key (conj k color)]]
         {new-key (->> (for [source (partition 2 1 new-key)]
                         (apply exclude source))
                       (apply combo/cartesian-product)
                       (map row)
                       (apply +))})
       (apply merge)))

(defn solve
  [n]
  (loop [i (dec n)
         row {[0] 1 [1] 1 [2] 1}]
    (if (zero? i)
      (->> (map val row)
           (apply +))
      (recur (dec i) (row-extend (flip row))))))

(comment
  (time (solve 8))
  )