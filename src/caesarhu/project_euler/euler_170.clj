(ns caesarhu.project-euler.euler-170
  (:require [clojure.math.combinatorics :as comb]
            [clojure.math.numeric-tower :as math]
            [caesarhu.shun-tools.math-misc :as misc]))

(defn is-pandigital? [coll]
  (= "0123456789" (apply str (sort coll))))

(defn valid-digits?
  [colls]
  (every? pos-int? (map first colls)))

(defn separate
  [result coll]
  (for [i (range 1 (count coll))
        :let [colls (split-at i coll)]
        :when (valid-digits? colls)]
    (concat result colls)))

(defn part
  [coll]
  (let [part-1 (separate [] coll)]
    (apply concat part-1 (for [[s1 s2] part-1]
                           (concat (separate [s1] s2) (separate [s2] s1))))))

(defn gcd
  [& ns]
  (loop [[x & xs] ns
         result x]
    (cond
      (nil? x) result
      (= result 1) 1
      :else (recur xs (math/gcd result x)))))

(defn get-pandigital
  [coll]
  (first (for [colls (separate [] coll)
               :let [numbers (map misc/to-number colls)
                     g (apply gcd numbers)]
               :when (#{3,6,9,12,15,18,21,24,27,36,39,45,48} g)
               gg (filter #(zero? (mod g %)) (range 3 (inc g) 3))
               :let [ds (map #(quot % gg) numbers)
                     digits (mapcat misc/digits (cons gg ds))]
               :when (is-pandigital? digits)]
           [(misc/to-number (apply concat colls)) gg ds numbers])))

(defn solve
  []
  (->> (comb/permutations (reverse (range 10)))
       (map get-pandigital)
       (remove empty?)
       first))

(defn solve2
  []
  (->> (comb/permutations [1 0 2 3 4 5 6 7 8 9])
       (map get-pandigital)
       (remove empty?)
       first))

(comment
  (time (solve))
  (time (solve2))
  (time (get-pandigital (misc/digits 1204739586)))
  (take 100 (comb/permutations (reverse (range 10)))))