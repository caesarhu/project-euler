(ns caesarhu.project-euler.euler-127
  (:require [caesarhu.shun-tools.primes :as p]
            [clojure.math.combinatorics :as comb]))

(defn rad
  [n]
  (apply * (distinct (p/prime-factors-of n))))

(defn radical
  [n]
  [(rad n) n])

(defn make-rad-map
  [rad-v]
  (->> (map #(sorted-map (first %) [(last %)]) rad-v)
       (apply merge-with concat)))

(defn abc-hits
  [limit]
  (let [rad-vec (vec (map radical (range 1 limit)))
        rad-map (make-rad-map rad-vec)
        rad-vec (vec (cons 0 (map first rad-vec)))
        get-rad (fn [x] (nth rad-vec x))]
    (for [rad-c (rest (keys rad-map))
          c (rest (rad-map rad-c))
          :let [c-half (/ c 2)]
          rad-a (take-while #(<= (* rad-c %) c-half) (range 1 (inc (quot c 2))))
          :when (p/coprime? rad-a rad-c)
          a (rad-map rad-a)
          :let [b (- c a)]
          :when (and (< a b)
                     (< (* rad-a rad-c (get-rad b)) c))]
      [a b c])))

(defn euler-127
  [limit]
  (->> (abc-hits limit)
       (map last)
       (apply +)))