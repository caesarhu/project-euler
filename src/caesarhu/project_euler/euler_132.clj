(ns caesarhu.project-euler.euler-132
  (:require [caesarhu.shun-tools.primes :as p]
            [clojure.math.numeric-tower :as math]))

(defn A
  [n]
  (when (p/coprime? 10 n)
    (loop [x 1, k 1]
      (if (zero? (mod x n))
        k
        (recur (mod (inc (* x 10)) n) (inc k))))))

(def prime-A-seq
  (->> (map #(vector % (A %)) p/primes)
       (remove #(nil? (last %)))))

(defn repunit-prime-factors
  [n]
  (->> (filter #(zero? (mod n (last %))) prime-A-seq)
       (map first)))

(defn euler-132
  [n m]
  (->> (take m (repunit-prime-factors n))
       (apply +)))

; (time (euler-132 (math/expt 10 9) 40))
