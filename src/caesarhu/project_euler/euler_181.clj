(ns caesarhu.project-euler.euler-181
  (:require [clojure.math.combinatorics :as combo]))

(defn count-partitions
  [n]
  ((reduce (fn [acc [x y]]
             (merge-with + acc {x (acc y)}))
           (apply merge (for [i (range (inc n))]
                          {i 1}))
           (for [i (range 2 (inc n))
                 j (range i (inc n))]
             [j (- j i)])) n))

(defn bipart-partitions
  [m n]
  (let [result (atom (vec (repeat (inc m) (vec (repeat (inc n) 1)))))]
    (doseq [white (range (inc n))
            black (range (inc m))
            :when (and (not= [white black] [0 0])
                       (not= [white black] [0 1])
                       (not= [white black] [1 0]))
            i (range white (inc n))
            j (range black (inc m))]
      (let [v-ji (get-in @result [j i])
            v-t (get-in @result [(- j black) (- i white)])]
        (swap! result assoc-in [j i] (+ v-ji v-t))))
    (get-in @result [m n])))

(defn bipart-partitions2
  [m n]
  (let [pos-seq (for [white (range (inc n))
                      black (range (inc m))
                      :when (and (not= [white black] [0 0])
                                 (not= [white black] [0 1])
                                 (not= [white black] [1 0]))
                      i (range white (inc n))
                      j (range black (inc m))]
                  [[j i] [(- j black) (- i white)]])]
    (loop [pos-seq pos-seq
           result (apply merge (for [i (range (inc m))
                                     j (range (inc n))]
                                 {[i j] 1}))]
      (if (empty? pos-seq)
        (result [m n])
        (let [[pos1 pos2] (first pos-seq)]
          (recur (rest pos-seq) (merge-with + result {pos1 (result pos2)})))))))

(defn solve
  []
  (bipart-partitions 60 40))

(comment
  (time (solve))
  (time (bipart-partitions2 60 40))
  )