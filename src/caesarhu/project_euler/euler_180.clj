(ns caesarhu.project-euler.euler-180
  (:require [clojure.math.numeric-tower :as math]
            [clojure.math.combinatorics :as combo]
            [injest.classical :refer [x>> =>>]]))

; https://projecteuler.net/thread=180;page=5
; rgiuli's solution

(defn square [n] (* n n))

(defn xy-fn
  [[x y]]
  [(+ x y)
   (/ 1 (+ (/ 1 x) (/ 1 y)))
   (math/sqrt (+ (square x) (square y)))
   (math/sqrt (/ 1 (+ (/ 1 (square x)) (/ 1 (square y)))))])

(defn k-set
  [k]
  (set (for [a (range 1 k)
             b (range (inc a) (inc k))]
         (/ a b))))

(defn solve
  [k]
  (let [ks (k-set k)
        valid-z? (fn [z] (ks z))
        get-xyz (fn [xy]
                (->> (xy-fn xy)
                     (filter valid-z?)
                     (map #(concat xy [%]))))]
    (x>> (combo/cartesian-product ks ks)
         (mapcat get-xyz)
         (map #(apply +' %))
         set
         (apply +')
         (#(+ (numerator %) (denominator %))))))

(comment
  (time (solve 35))
  )