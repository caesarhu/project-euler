(ns caesarhu.project-euler.euler-146
  (:require [caesarhu.shun-tools.primes :as p]
            [caesarhu.shun-tools.miller-rabin :as rabin]))

(def prime-pattern [1 3 7 9 13 27])
(def composite-pattern [21])
(def remainder-pattern [10, 80, 130, 200])

(defn consecutive-primes?
  ([n]
   (consecutive-primes? n 0))
  ([n r]
   (let [n+r-2 (let [x (+ n r)]
                 (* x x))]
     (and (every? rabin/deterministic-test (map #(+ n+r-2 %) prime-pattern))
          (every? (complement rabin/deterministic-test) (map #(+ n+r-2 %) composite-pattern))))))

(defn euler-146
  [limit]
  (apply + (for [n (range 0 limit 210)
                 r remainder-pattern
                 :when (consecutive-primes? n r)]
             (+ n r))))

; (time (euler-146 150000000))