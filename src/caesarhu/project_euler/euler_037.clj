(ns caesarhu.project-euler.euler-037
  (:require [caesarhu.shun-tools.math-misc :as misc]
            [caesarhu.shun-tools.primes :as p]))

(defn truncate
  [p]
  (let [p-digits (misc/digits p)]
    (->> (for [i (range (count p-digits))]
           (->> [(drop i p-digits) (drop-last i p-digits)]
                (map misc/to-number)))
         (apply concat)
         (distinct))))

(defn truncatable?
  [p]
  (every? p/is-prime? (truncate p)))

(defn valid-digits?
  [p]
  (or (= p 23)
      (every? odd? (misc/digits p))))

(defn solve
  []
  (->> (p/primes-after 10)
       (filter valid-digits?)
       (filter truncatable?)
       (take 11)
       (apply +)))

(comment
  (time (solve))
  )