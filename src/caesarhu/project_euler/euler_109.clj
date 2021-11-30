(ns caesarhu.project-euler.euler-109
  (:require [caesarhu.shun-tools.math-misc :as misc]
            [caesarhu.shun-tools.primes :as p]
            [clojure.math.numeric-tower :as math]))

(def single-darts (concat (range 1 21) [25]))
(def double-darts (map #(* 2 %) single-darts))
(def triple-darts (map #(* 3 %) (butlast single-darts)))
(def all-darts (sort (concat single-darts double-darts triple-darts)))

(defn solve
  [limit]
  (let [double-seq (filter #(< % limit) double-darts)
        miss-1 (count (for [d double-seq
                            i all-darts
                            :when (< (+ d i) limit)]
                        1))
        miss-0 (count (for [d double-seq
                            i (range (count all-darts))
                            j (range i (count all-darts))
                            :let [ix (nth all-darts i)
                                  jx (nth all-darts j)]
                            :when (< (+ ix jx d) limit)]
                        1))]
    (+ (count double-seq) miss-1 miss-0)))

(comment
  all-darts
  (solve 100)
  )