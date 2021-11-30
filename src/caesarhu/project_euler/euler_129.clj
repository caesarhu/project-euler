(ns caesarhu.project-euler.euler-129
  (:require [caesarhu.shun-tools.primes :as p]))

(defn A
  [n]
  (if (p/coprime? 10 n)
    (loop [x 1, k 1]
      (if (zero? (mod x n))
        k
        (recur (mod (inc (* x 10)) n) (inc k))))
    0))

(defn euler-129
  [limit]
  (let [start (some #(and (odd? %) %) (iterate inc (inc limit)))
        target-seq (iterate #(+ 2 %) start)]
    (some #(and (> (A %) limit) %) target-seq)))

(comment
  (time (euler-129 1000000))
  )
