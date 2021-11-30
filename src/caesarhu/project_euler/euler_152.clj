(ns caesarhu.project-euler.euler-152
  (:require [caesarhu.shun-tools.primes :as p]
            [clojure.math.numeric-tower :as math]
            [clojure.math.combinatorics :as comb]))

(def target 1/2)

(defn square
  [n]
  (*' n n))

(def base-seed [2 3 4])

(defn lcm
  [coll]
  (if (empty? coll)
    0
    (reduce math/lcm coll)))

(defn square-sum
  [seed-lcm2 coll]
  (apply + (map #(quot seed-lcm2 (square %)) coll)))

(defn prime-subsets
  ([limit seed prime-or-power coll]
   (let [prime (first (p/prime-factors-of prime-or-power))
         prime2 (square prime)
         seed-lcm (lcm seed)
         multipliers (->> (filter #(and (p/coprime? % prime-or-power)
                                        (zero? (mod seed-lcm %)))
                                  (range 1 (inc (quot limit prime-or-power))))
                          (map (partial * prime-or-power)))
         lcm2 (square seed-lcm)]
     (for [c coll
           :let [c-set (set c)
                 m (remove c-set multipliers)]
           subset (comb/subsets m)
           :let [prime-subset (concat subset c)
                 sum (square-sum lcm2 prime-subset)]
           :when (zero? (mod sum prime2))]
       prime-subset)))
  ([limit seed-lcm prime-or-power]
   (prime-subsets limit seed-lcm prime-or-power [[]])))

(defn get-seed
  [limit]
  (reduce (fn [seed prime-or-power]
            (if (second (prime-subsets limit (conj seed prime-or-power) prime-or-power))
              (conj seed prime-or-power)
              seed))
          base-seed
          (filter #(= 1 (count (frequencies (p/prime-factors-of %))))
                  (range 5 (quot limit 2)))))

(defn smooth-numbers
  [seed]
  (let [primes (->> (remove p/is-prime? seed)
                    (map #(first (p/prime-factors-of %)))
                    distinct)
        smooth-seed (->> (for [p primes]
                           (butlast (filter #(zero? (mod % p)) seed)))
                         ((fn [coll]
                            (sort (set (apply concat base-seed coll))))))]
    (loop [ps (reverse smooth-seed)
           result []]
      (if (< (count ps) 2)
        [smooth-seed (sort result)]
        (let [prime (first ps)
              smooth (concat (if (p/is-prime? prime)
                               []
                               [prime])
                             (for [subset (comb/cartesian-product [prime] (rest ps))
                                   :when (apply p/coprime? subset)]
                               (apply * subset)))]
          (recur (rest ps) (concat result smooth)))))))

(defn get-solution
  [seed coll]
  (let [base-prime [2 3]
        smooth-numbers (last (smooth-numbers seed))
        lcm2 (square (lcm seed))
        target-lcm (* target lcm2)
        extra-map (->> (map #(concat base-prime %) (comb/subsets smooth-numbers))
                       (map #(hash-map (square-sum lcm2 %) %))
                       (apply merge))]
    (when-let [match (extra-map (- target-lcm (square-sum lcm2 coll)))]
      (sort (concat match coll)))))

(defn solve
  [limit]
  (let [seed (get-seed limit)
        smooth-seed (set (first (smooth-numbers seed)))
        prime-or-power (->> (remove smooth-seed seed)
                            (sort-by #(first (p/prime-factors-of %)))
                            reverse)
        possible (reduce (fn [acc p]
                           (prime-subsets limit seed p acc))
                         [[]] prime-or-power)]
    (reduce (fn [solutions p]
              (if-let [solution (get-solution seed p)]
                (conj solutions solution)
                solutions))
            [] possible)))

(defn euler-152
  [limit]
  (count (solve limit)))

; (time (euler-152 80))

