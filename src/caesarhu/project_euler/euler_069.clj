(ns caesarhu.project-euler.euler-069
  (:require [caesarhu.shun-tools.primes :as p]
            [clojure.math.numeric-tower :as math]))

; 因為Euler's Totient function, φ(n)是指小於n且與n互質之數的個數，
; 所以最小的φ(n)就是包含最多質數的那個數。

(defn max-primes-factors-of
  [limit]
  (loop [counter 1]
    (let [sum (apply * (take counter p/primes))]
      (if (> sum limit)
        (take (dec counter) p/primes)
        (recur (inc counter))))))

(defn solve
  [limit]
  (apply * (max-primes-factors-of limit)))