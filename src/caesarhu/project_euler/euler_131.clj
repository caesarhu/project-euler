(ns caesarhu.project-euler.euler-131
  (:require [caesarhu.shun-tools.primes :as p]))

(def cube-seq
  (map #(* % % %) (rest (range))))

(defn lazy-limit
  [limit [a b :as s]]
  (lazy-seq
    (if (< (- b a) limit)
      (cons a (lazy-limit limit (rest s)))
      [a])))

(defn cube-partnership
  [limit]
  (let [cubes (lazy-limit limit cube-seq)]
    (for [i (rest cubes)
          j cubes
          :when (and (> i j)
                     (< (- i j) limit))
          :let [p (- i j)]
          :when (p/is-prime? p)]
      p)))

(defn euler-131
  [limit]
  (count (cube-partnership limit)))

