(ns caesarhu.project-euler.euler-010
  (:require [caesarhu.shun-tools.primes :refer [primes-range]]
            [clojure.math.numeric-tower :as math]))

(defn sieve
  [n]
  (->> (primes-range 0 n)
       (apply +)))

(defn solve
  [n]
  (let [[r _] (math/exact-integer-sqrt n)
        v (concat (for [i (range 1 (inc r))]
                    (quot n i))
                  (range (dec r) 0 -1))
        s (atom (into {} (for [i v]
                           [i (dec (quot (* i (inc i)) 2))])))]
    (doseq [p (range 2 (inc r))
            :let [sp (@s (dec p))
                  p2 (* p p)]
            :when (> (@s p) sp)
            i v :when (>= i p2)]
      (swap! s #(merge-with - %1 %2) {i (* p (- (@s (quot i p)) sp))}))
    (@s n)))

(comment
  (time (solve 20000000))
  (time (sieve 20000000))
  )