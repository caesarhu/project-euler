(ns caesarhu.project-euler.euler-172
  (:require [clojure.math.combinatorics :as combo]
            [caesarhu.shun-tools.math-misc :as misc]))

(defn zero-heads-ratio
  [coll]
  (/ (count (filter zero? coll)) (count coll)))

(defn count-permutations
  [coll]
  (* (combo/count-permutations coll) (- 1 (zero-heads-ratio coll))))

(defn brute-force
  [n-digits n-repeat]
  (let [base (range 10)
        base-vector (apply concat (repeat n-repeat base))]
    (->> (combo/combinations base-vector n-digits)
         (map count-permutations)
         (apply +'))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn next-sum-map
  [m]
  (->> (for [[s v] m
             i (range (dec (count s)))
             :when (pos-int? (nth s i))]
         (let [k (vec (for [j (range (count s))
                            :let [jv (nth s j)]]
                        (cond
                          (= i j) (dec jv)
                          (= (inc i) j) (inc jv)
                          :else jv)))]
           {k (* v (nth s i))}))
       (apply merge-with +')))

(defn generate-sums
  [n]
  (loop [i (dec n)
         sum-map {[9 1 0 0] 9}]
    (if (zero? i)
      sum-map
      (recur (dec i) (next-sum-map sum-map)))))

(defn solve
  [n]
  (->> (generate-sums n)
       vals
       (apply +')))

(comment
  (time (solve 18))
  (time (brute-force 18 3))
  )