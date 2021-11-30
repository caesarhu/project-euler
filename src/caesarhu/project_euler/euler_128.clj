(ns caesarhu.project-euler.euler-128
  (:require [caesarhu.shun-tools.primes :as p]))

(def ring-seq
  (iterate (fn [[ring start-num step]]
             [(inc ring) (+ start-num step) (+ step 6)]) [1 2 6]))

(defn pd-3
  []
  (map (fn [[ring start-num step]]
         (->> [(when (and (p/is-prime? (inc (* 6 ring)))
                          (p/is-prime? (dec (* 6 ring)))
                          (p/is-prime? (+ (* 12 ring) 5)))
                 start-num)
               (when (and (p/is-prime? (dec (* 6 ring)))
                          (p/is-prime? (- (* 12 ring) 7))
                          (p/is-prime? (+ (* 6 ring) 5))
                          (> ring 1))
                 (dec (+ start-num step)))]
              (remove nil?)))
       ring-seq))

(def pd-3-seq
  (concat [0 0] (flatten (pd-3))))

(defn euler-128
  [target]
  (nth pd-3-seq target))
