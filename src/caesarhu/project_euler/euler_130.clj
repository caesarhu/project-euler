(ns caesarhu.project-euler.euler-130
  (:require [caesarhu.shun-tools.primes :as p]))

(defn A
  [n]
  (when (p/coprime? 10 n)
    (loop [x 1, k 1]
      (if (zero? (mod x n))
        k
        (recur (mod (inc (* x 10)) n) (inc k))))))

(def composite? (complement p/is-prime?))

(defn composite-n?
  [n]
  (and (composite? n)
       (when-let [a (A n)]
         (zero? (mod (dec n) a)))))

(def composite-seq
  (filter composite-n? (iterate #(+ 2 %) 3)))

(defn euler-130
  [limit]
  (->> (take limit composite-seq)
       (apply +)))
