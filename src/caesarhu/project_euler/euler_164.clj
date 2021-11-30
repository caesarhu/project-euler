(ns caesarhu.project-euler.euler-164
  (:require [clojure.math.numeric-tower :as math]
            [caesarhu.shun-tools.math-misc :as misc]))

(def max-sum 9)

(defn valid?
  [n]
  (if (< n 100)
    (->> (misc/digits n)
         (apply +)
         (>= max-sum))
    (->> (misc/digits n)
         (partition 3 1)
         (map #(apply + %))
         (every? #(<= % max-sum)))))

(defn brute-force
  [n]
  (let [start (math/expt 10 (dec n))
        limit (math/expt 10 n)]
    (->> (range start limit)
         (filter valid?)
         count)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def init-map
  (frequencies (map vector (range 10))))

(defn next-map
  [m]
  (->> (for [i (range 10)
             [[a b] n] m
             :when (<= (+ i a (if b b 0)) 9)]
         {[i a] n})
       (apply merge-with +')))

(defn count-map
  [m]
  (apply + (for [[[a _] n] m
                 :when (not (zero? a))]
             n)))

(defn solve
  [digits]
  (loop [d 1
         m init-map]
    (if (>= d digits)
      (count-map m)
      (recur (inc d) (next-map m)))))

(defn euler-164
  []
  (time (solve 20)))