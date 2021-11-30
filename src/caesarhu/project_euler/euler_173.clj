(ns caesarhu.project-euler.euler-173
  (:require [clojure.math.numeric-tower :as math]))

(defn square
  [n]
  (* n n))

(defn brute-force
  [limit]
  (->> (for [i (range 3 (inc (/ (inc limit) 4)))
             j (range 1 i)
             :let [target (- (square i) (square j))]
             :when (and (<= target limit)
                        (zero? (mod target 4)))]
         1)
       (apply +)))

(defn root
  [limit x]
  (let [r (- (square x) (inc limit))]
    (if (pos-int? r)
      (first (math/exact-integer-sqrt r))
      0)))

(defn brute-force2
  [limit]
  (reduce + (for [i (range 3 (+ (quot limit 4) 2))]
              (let [r (root limit i)]
                (quot (dec (- i r)) 2)))))

(defn solve
  [limit]
  (let [bound (/ limit 4)]
    (loop [i 1
           result 0]
      (if (>= (square i) bound)
        result
        (let [current (- (quot bound i) i)]
          (recur (inc i) (+' result current)))))))

(comment
  (time (solve (math/expt 10 6)))
  (time (brute-force2 (math/expt 10 6)))
  (time (brute-force 25))
  )