(ns caesarhu.project-euler.euler-095
  (:require [caesarhu.shun-tools.primes :as p]
            [caesarhu.shun-tools.math-misc :as misc]
            [clojure.math.numeric-tower :as math]))

(defn sum-of-divisors
  [n]
  (if (p/is-prime? n)
    1
    (let [primes (frequencies (p/prime-factors-of n))
          sum (apply * (for [[p exp] primes]
                         (apply + (for [e (range (inc exp))]
                                    (math/expt p e)))))]
      (- sum n))))

(defn amicable-chain
  [limit n]
  (loop [s (iterate sum-of-divisors n)
         chain []
         chain-set #{}]
    (let [f (first s)]
      (cond
        (> f limit) []
        (< f n) []
        (chain-set f) (if (= (first chain) f)
                        chain
                        [])
        :else (recur (rest s) (conj chain f) (conj chain-set f))))))

(defn brute-force
  [limit]
  (->> (for [i (take-while #(< % limit) p/composites)
             :let [chain (amicable-chain limit i)
                   length (count chain)]]
         {length i})
       (apply merge-with min)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn vec-add!
  [atom-v i value]
  (swap! atom-v assoc i (+ (@atom-v i) value)))

(defn generate-sum
  [limit]
  (let [v (atom (vec (repeat (inc limit) 0)))]
    (doseq [i (range 1 (inc (/ limit 2)))]
      (doseq [j (range (* 2 i) (inc limit) i)]
        (vec-add! v j i)))
    @v))

(defn solve
  [limit]
  (let [sum-vector (generate-sum limit)]
    (loop [[n & other] p/composites
           visited #{}
           result nil]
      (cond
        (> n limit) (first result)
        (visited n) (recur other visited result)
        :else (let [[is-chain? chain] (loop [n n
                                             chain []]
                                        (let [new-chain (conj chain n)]
                                          (cond
                                            (or (> n limit) (visited n)) [false new-chain]
                                            (some #(= % n) chain) [true new-chain]
                                            :else (recur (sum-vector n) new-chain))))
                    new-visited (into visited chain)
                    new-result (if is-chain?
                                 (max-key count result (butlast (drop-while #(not= (last chain) %) chain)))
                                 result)]
                (recur other new-visited new-result))))))

(comment
  (time (solve 1000000))
  (time (brute-force 1000000))
  )