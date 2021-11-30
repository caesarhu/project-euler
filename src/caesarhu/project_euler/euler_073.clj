(ns caesarhu.project-euler.euler-073
  (:require [clojure.math.numeric-tower :as math]
            [caesarhu.shun-tools.primes :as p]))

(defn between-fraction
  [[begin end] n]
  (let [n*begin (* n begin)
        n*end (* n end)
        begin-n (if (= n*begin (int n*begin))
                  (int (inc n*begin))
                  (math/ceil n*begin))
        end-n (if (= n*end (int n*end))
                (int (dec n*end))
                (math/floor n*end))]
    (tap> begin-n)
    (tap> end-n)
    (for [i (range begin-n (inc end-n))
          :when (p/coprime? i n)]
      (/ i n))))

(defn solve
  [limit]
  (->> (range 1 (inc limit))
       (mapcat #(between-fraction [1/3 1/2] %))
       count))