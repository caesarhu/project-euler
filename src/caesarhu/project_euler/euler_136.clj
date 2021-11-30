(ns caesarhu.project-euler.euler-136
  (:require [caesarhu.shun-tools.primes :as p]))

(defn euler-136
  [limit]
  (let [counter (atom 0)
        primes (p/primes-range 0 limit)
        _ (doseq [p primes
                  :let [n (cond->
                            0
                            (< p (/ limit 4)) (inc)
                            (< p (/ limit 16)) (inc)
                            (= 3 (mod p 4)) (inc))]]
            (swap! counter + n))]
    @counter))

