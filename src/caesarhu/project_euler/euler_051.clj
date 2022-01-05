(ns caesarhu.project-euler.euler-051
  (:require [caesarhu.primes :as p]
            [caesarhu.shun-tools.math-misc :as misc]))

(defn unique-digits
  [n]
  (-> (misc/digits n) distinct))

(defn replace-digit
  [n d]
  (->> (for [i (range 10)]
         (for [j (misc/digits n)]
           (if (= j d) i j)))
       (filter #(pos-int? (first %)))
       (map misc/to-number)))

(defn count-primes
  [s]
  (->> (filter p/is-prime? s)
       count))

(defn is-target?
  [t n]
  (->> (map #(replace-digit n %) (unique-digits n))
       (some #(>= (count-primes %) t))))

(defn solve-051
  [n]
  (some #(and (is-target? n %) %) p/primes))

(comment
  (time (solve-051 8))
  )

