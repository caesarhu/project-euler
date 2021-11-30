(ns caesarhu.project-euler.euler-150
  (:require [clojure.math.numeric-tower :as math]))

(def mod-base (math/expt 2 20))
(def minus-base (/ mod-base 2))

(defn next-t
  [t]
  (mod (+ (* 615949 t) 797807) mod-base))

(defn t->s
  [t]
  (- t minus-base))

(defn pseudo-vector
  [height]
  (let [s (map t->s (rest (iterate next-t 0)))]
    (first (reduce (fn [[result s] i]
                     (let [[row other] (split-at i s)]
                       [(conj result (vec row)) other]))
                   [[] s] (range 1 (inc height))))))

(defn sums-vector
  [coll]
  (vec (for [row coll]
         (vec (reductions + row)))))

(defn solve
  [height]
  (let [triangle (sums-vector (pseudo-vector height))
        row-subsum (fn [i j1 j2]
                     (- (get-in triangle [i j2]) (if (zero? j1) 0 (get-in triangle [i (dec j1)]))))
        pos-sums (fn [[i j]]
                   (apply min (reductions + (for [k (range (- height i))]
                                              (row-subsum (+ i k) j (+ k j))))))
        all-pos (for [i (range height)
                      j (range (inc i))]
                  [i j])]
    (apply min (map pos-sums all-pos))))

(defn euler-150
  [limit]
  (let [triangle (pseudo-vector limit)]
    (loop [i (int (- limit 1))
           res []
           min-so-far (long 99999999)]
      (if (neg? i)
        min-so-far
        (if (== i (- limit 1))
          (let [nres (mapv #(hash-map :all [(get-in triangle [i %])]
                                      :right [(get-in triangle [i %])])
                           (range limit))]
            (do (println min-so-far)
                (recur (- i 1) nres (apply min (map #(first (:all %)) nres)))))
          (let [nres (mapv #(let [value (get-in triangle [i %])]
                              (hash-map :all (->> (map (fn [a b] (+ value a b))
                                                       (:all (res %))
                                                       (:right (res (+ % 1))))
                                                  (cons value)
                                                  vec)
                                        :right (->> (map (fn [a] (+ value a))
                                                         (:right (res (+ % 1))))
                                                    (cons value)
                                                    vec)))
                           (range (+ i 1)))]
            (do (println i)
                (recur (- i 1) nres (->> (map #(apply min (:all %)) res)
                                         (apply min)
                                         (min min-so-far))))))))))
