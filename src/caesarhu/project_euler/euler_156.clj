(ns caesarhu.project-euler.euler-156
  (:require [caesarhu.shun-tools.math-misc :as misc]
            [clojure.math.numeric-tower :as math]))

; https://projecteuler.net/thread=156;page=7#last

(defn count-d
  [n d]
  (count (filter (partial = d) (misc/digits n))))

(defn sum-digit
  [n d]
  (let [digits (vec (reverse (misc/digits n)))
        length (count digits)]
    (loop [i 0
           remainder 0
           result 0]
      (if (>= i length)
        result
        (let [ix (get digits i)
              exp (math/expt 10 i)
              sub-d (if (zero? i) 0 (* i ix (quot exp 10)))
              current (cond
                        (> ix d) exp
                        (< ix d) 0
                        :else (inc remainder))]
          (recur (inc i) (+ remainder (* ix exp)) (+ result sub-d current)))))))

(def limit (math/expt 10 11))

(defn solutions
  [d]
  (loop [n 1
         jump 0
         answers 0
         result 0]
    (if (>= n limit)
      [result answers jump]
      (let [excess (- n (sum-digit n d))]
        (if (zero? excess)
          (recur (inc n) (inc jump) (inc answers) (+ result n))
          (let [skip (max 1 (quot (* 9 (math/abs excess)) (* 10 (inc (count-d n d)))))]
            (recur (+ n skip) (inc jump) answers result)))))))

(defn solve
  []
  (->> (map solutions (range 1 10))
       (map first)
       (apply +)))

(defn euler-156
  []
  (time (solve)))