(ns caesarhu.project-euler.euler-027
  (:require [caesarhu.shun-tools.primes :refer [primes is-prime?]]))

(defn quadratic-equation
  [a b]
  (let [quadratic (fn [n]
                    (+ (* n n) (* a n) b))]
    (->> (map quadratic (range))
         (take-while is-prime?)
         count)))

(defn quadratic-seq
  [n]
  (let [a-range (range (inc (- n)) n)
        b-range (take-while #(< % n) primes)]
    (apply max-key first (for [a a-range
                               b b-range]
                           [(quadratic-equation a b) [a b]]))))

(defn solve
  [n]
  (->> (quadratic-seq n)
       last
       (apply *)))

; (time (solve 1000))
