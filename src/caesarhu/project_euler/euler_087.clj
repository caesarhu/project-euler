(ns caesarhu.project-euler.euler-087
  (:require [clojure.math.numeric-tower :as math]
            [caesarhu.shun-tools.primes :as p]))

(def limit (* 50 (math/expt 10 6)))

(defn power-primes
  [p]
  (map #(math/expt % p) p/primes))

(defn solve
  [top]
  (count (into #{}
               (comp
                (mapcat (fn [n] (map #(+ n %) (take-while #(< (+ n %) top) (power-primes 3)))))
                (mapcat (fn [n] (map #(+ n %) (take-while #(< (+ n %) top) (power-primes 2))))))
               (take-while #(< % top) (power-primes 4)))))

(comment
  (time (solve limit))
  )