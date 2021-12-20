(ns caesarhu.project-euler.euler-043
  (:require [caesarhu.shun-tools.math-misc :as misc]))

(defn is-different?
  [v]
  (= (count v) (count (set v))))

(defn is-divisible?
  [v d]
  (-> (take-last 3 v) misc/to-number (mod d) zero?))

(def init (map vector (range 1 10)))
(def digit (range 10))
(def primes [1 1 2 3 5 7 11 13 17])

(defn prod-digit
  [s]
  (mapcat #(map (fn [d]
                  (concat % [d])) digit) 
          s))

(defn next-digit
  [s prime]
  (->> (prod-digit s)
       (filter is-different?)
       (filter #(is-divisible? % prime))))

(defn solve
  []
  (->> (reduce (fn [acc prime]
                 (next-digit acc prime))
               init primes)
       (map misc/to-number)
       (apply +)))

(comment
  (time (solve))
  )