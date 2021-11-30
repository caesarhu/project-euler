(ns caesarhu.project-euler.euler-159
  (:require [caesarhu.shun-tools.math-misc :as misc]
            [caesarhu.shun-tools.primes :as p]
            [clojure.math.numeric-tower :as math]
            [clojure.math.combinatorics :as comb]))

(defn digital-root
  [n]
  (let [n-mod-9 (mod n 9)]
    (if (zero? n-mod-9) 9
        n-mod-9)))

(defn mdrs
  [n]
  (->> n
       (p/prime-factors-of)
       (comb/partitions)
       (map (fn [coll]
              (map #(apply * %) coll)))
       (map (fn [coll]
              (map digital-root coll)))
       (map #(apply + %))
       (apply max)))

(defn brute-force
  [limit]
  (apply + (map mdrs (range 2 limit))))

(defn solve
  []
  (let [start 2 stop 1000000]
    (loop [n    start
           mdrs (zipmap (range 2 stop) (map digital-root (range 2 stop)))]
      (if (= n stop) (reduce + (vals mdrs))
          (recur (inc n)
                 (merge-with max mdrs
                             (apply merge
                                    (for [mult-of-n (range (+ n n) stop n)
                                          :let [this (get mdrs n)
                                                that (get mdrs (/ mult-of-n n))]]
                                      {mult-of-n (+ this that)}))))))))

(defn euler-159
  []
  (time (solve)))