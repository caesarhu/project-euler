(ns caesarhu.project-euler.euler-088
  (:require [caesarhu.shun-tools.primes :as p]
            [clojure.math.combinatorics :as comb]
            [criterium.core :refer [bench]]))

(defn calc-factors
  [n]
  (->> (rest (comb/partitions (p/prime-factors-of n)))
       (map (fn [coll]
              (map #(apply * %) coll)))))

(defn make-k
  [n]
  (for [fs (calc-factors n)
        :let [s (apply + fs)
              c (count fs)
              k (+ n (- s) c)]]
    {k n}))

(defn euler-088
  [limit]
  (->> (apply concat (map make-k (range 2 (inc (* limit 2)))))
       (apply merge-with min)
       ((fn [m]
          (map #(m %) (range 2 (inc limit)))))
       set
       (apply +)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn generate-product-sum-map
  [limit]
  (let [product-sum-map (atom {})]
    (letfn [(product-sum
             [product sum num start]
             (let [k (+ (- product sum) num)]
               (when (< k limit)
                 (swap! product-sum-map #(merge-with min %1 %2) {k product})
                 (doseq [i (range start (inc (* (quot limit product) 2)))]
                   (product-sum (* product i) (+ sum i) (inc num) i)))
               @product-sum-map))]
      (product-sum 1 1 1 2))))

(defn solve
  [limit]
  (->> (generate-product-sum-map (inc limit))
       (#(dissoc % 1))
       vals
       distinct
       (apply +)))

(comment
  (time (solve 12000))
  (time (euler-088 12000))
  )