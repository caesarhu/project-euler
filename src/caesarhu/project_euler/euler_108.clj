(ns caesarhu.project-euler.euler-108
  (:require [caesarhu.shun-tools.primes :as p]
            [clojure.math.numeric-tower :as math]))

(defn square-divisors
  [n]
  (->> (p/prime-factors-of n)
       frequencies
       vals
       (map #(inc (* 2 %)))
       (apply *)
       (#(math/ceil (/ % 2)))))

(defn brute-force
  [limit]
  (first (drop-while #(<= (square-divisors %) limit) (iterate inc 2))))

(comment
  (time (brute-force 1000))
  )
