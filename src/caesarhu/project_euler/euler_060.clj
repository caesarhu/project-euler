(ns caesarhu.project-euler.euler-060
  (:require [caesarhu.shun-tools.math-misc :as misc]
            [caesarhu.shun-tools.primes :as p]))

(def concatenated-primes
  (memoize
    (fn [n limit]
      (let [d (misc/digits n)]
        (set
          (for [a (p/primes-range 2 limit)
                :when (and
                        (p/is-prime? (misc/to-number (concat d (misc/digits a))))
                        (p/is-prime? (misc/to-number (concat (misc/digits a) d))))]
            a))))))

(defn subset [limit & ks]
  (let [s (apply clojure.set/intersection (map #(concatenated-primes % limit) ks))]
    (apply (partial disj s) ks)))

(defn solve [limit]
  (first
    (for [a (p/primes-range 2 limit)
          b (subset limit a)
          c (subset limit a b)
          d (subset limit a b c)
          e (subset limit a b c d)
          :let [s [a b c d e]]]
      {:sum (reduce + s) :primes s})))

; (time (solve 10000))