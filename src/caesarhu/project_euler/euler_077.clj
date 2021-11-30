(ns caesarhu.project-euler.euler-077
  (:require [caesarhu.shun-tools.primes :as p]
            [caesarhu.shun-tools.math-misc :as misc]))

(def integer-sum
  (memoize
    (fn [c v]
      (let [f (first c)]
        (cond
          (nil? f) 0
          (and (= f (last c)) (zero? (mod v f))) 1
          :else (reduce +
                        (for [n (range 0 (inc (quot v f)))]
                          (integer-sum (rest c) (- v (* n f))))))))))

(defn get-ways [n]
  (let [p (reverse (take-while #(< % n) p/primes))]
    (integer-sum p n)))

(defn solve [n]
  (first
    (filter #(> (get-ways %) n) misc/integers)))

; (time (solve 5000))

(defn -prime-addition-ways
  [n prime-seq]
  (cond
    (zero? n) 1
    (or (< n 0) (empty? prime-seq)) 0
    :else (+ (-prime-addition-ways n (rest prime-seq))
             (-prime-addition-ways (- n (first prime-seq)) prime-seq))))

(defn prime-addition-ways
  [n]
  (-prime-addition-ways n (p/primes-range 0 n)))

(defn solve [n]
  (first
    (filter #(> (prime-addition-ways %) n) misc/integers)))