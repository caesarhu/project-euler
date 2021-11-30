(ns caesarhu.project-euler.euler-187
  (:require [caesarhu.shun-tools.primes :as p]
            [criterium.core :refer [bench quick-bench]]))

(defn solve-slow
  [limit]
  (let [take-prime (fn [primes p]
                     (->> (drop-while #(< % p) primes)
                          (take-while #(< % (/ limit p)))))]
    (last (reduce (fn [[primes sum] p]
                    (let [new-primes (take-prime primes p)]
                      [new-primes (+ sum (count new-primes))]))
                  [(p/primes-tox limit) 0]
                  (p/primes-tox (Math/sqrt limit))))))

(defn solve
  [limit]
  (loop [primes (vec (p/primes-tox (/ limit 2)))
         sum 0]
    (let [counter (count primes)]
      (cond
        (zero? counter) sum
        (= counter 1) (inc sum)
        :else (let [upper (/ limit (second primes))
                    i (if-let [x (some #(and (< (primes %) upper) %) (iterate dec (dec counter)))] x 0)
                    new-primes (subvec primes 1 (inc i))]
                (recur new-primes (+ sum counter)))))))

(comment
  (quick-bench (solve 100000000))
  (time (solve 100000000))
  )