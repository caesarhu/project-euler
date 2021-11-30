(ns caesarhu.project-euler.euler-154-new
  (:require [caesarhu.shun-tools.math-misc :as misc]))

(defn sum-of-digits
  [n p]
  (reduce + (misc/digits n p)))

(def digits-limit 200000)
(def digits-sum
  (atom (into [] (for [i (range (inc digits-limit))]
                   [(sum-of-digits i 5)
                    (sum-of-digits i 2)]))))

(defn kummer-atom
  [coll]
  (let [sum (reduce + coll)
        args-sum (->> (map #(@digits-sum %) coll)
                      (apply map +))
        [f5 f2] (map - args-sum (@digits-sum sum))]
    [(quot f5 4) f2]))

(defn kummer
  [prime coll]
  (let [digits-sum (reduce + (mapcat #(misc/digits % prime) coll))
        sum-digits (reduce + (misc/digits (reduce + coll) prime))]
    (quot (- digits-sum
             sum-digits)
          (dec prime))))

(defn count-valid
  [[i j k]]
  (cond
    (= i j k) 1
    (or (= i j) (= j k) (= i k)) 3
    :else 6))

(defn solve
  [limit power-of-10]
  (let [valid? (fn [coll]
                 (let [[f5 f2] (kummer-atom coll)]
                   (and (>= f5 power-of-10)
                        (>= f2 power-of-10))))]
    (loop [i 0
           j i
           result 0]
      (cond 
        (> i (quot limit 3)) result
        (> j (quot (- limit i) 2)) (recur (inc i) (inc i) result)
        :else (let [k (- limit i j)
                    valid-count (if (valid? [i j k])
                                  (count-valid [i j k])
                                  0)]
                (recur i (inc j) (+ result valid-count)))))))
