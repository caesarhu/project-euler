(ns caesarhu.project-euler.euler-050
  (:require [caesarhu.shun-tools.primes :as p]
            [caesarhu.shun-tools.math-misc :as misc]))

(defn consecutive-primes
  [limit]
  (loop [counter (range)]
    (let [sum (apply + (take (first counter) p/primes))]
      (if (> sum limit)
        (take (dec (first counter)) p/primes)
        (recur (rest counter))))))

(defn find-sum-prime
  [prime-seq]
  (let [sum-prime? (fn [coll] (p/is-prime? (apply + coll)))]
    (->> (mapcat #(partition % 1 prime-seq) (range (count prime-seq) 0 -1))
         (some #(and (sum-prime? %) %)))))

(defn solve
  [limit]
  (->> (consecutive-primes limit)
       (find-sum-prime)
       (#(hash-map :sum (apply + %) :values %))))

;; This is better than euler-19 accum, which wasn't lazy
(defn make-seq-accumulator [seq]
  (map first (iterate (fn [[sum s]]
                        [(+ sum (first s)) (next s)])
                      [(first seq) (rest seq)])))

(def prime-sums (conj (make-seq-accumulator p/primes) 0))

(defn euler-50 [goal]
  (loop [c 1]
    (let [bots (reverse (take c prime-sums))
          tops (take c (reverse (take-while #(> goal (- % (last bots)))
                                            (rest prime-sums))))]
      (if-let [v (some #(if (p/is-prime? %) % nil)
                       (map #(- %1 %2) tops bots))]
        v
        (recur (inc c))))))

(comment
  (time (solve 1000000))
  (time (count (p/primes-tox 1000000))))
