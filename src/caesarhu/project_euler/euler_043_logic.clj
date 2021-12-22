(ns caesarhu.project-euler.euler-043-logic
  (:refer-clojure :exclude [==])
  (:require [clojure.core.logic :refer :all]
            [clojure.core.logic.fd :as fd]
            [caesarhu.shun-tools.math-misc :as misc]))

(defn d3-divisible
  [a b c p]
  (fd/eq
   (= (* (/ (+ (* a 100) (* b 10) c) p) p) (+ (* a 100) (* b 10) c))))

(defn logic
  []
  (let [vars (vec (repeatedly 10 lvar))
        primes [2 3 5 7 11 13 17]
        rules (vec (for [i (range (count primes))]
                     (conj (subvec vars (inc i) (+ i 4)) (primes i))))]
    (run* [q]
          (== q vars)
          (everyg #(fd/in % (fd/interval 9)) vars)
          (fd/distinct vars)
          (fd/> (vars 0) 0)
          (everyg #(apply d3-divisible %) rules))))

(defn solve
  []
  (->> (logic)
       (map misc/to-number)
       (apply +)))

(comment
  (logic)
  (time (solve))
  )