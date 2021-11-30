(ns caesarhu.project-euler.euler-090
  (:require [clojure.math.combinatorics :as comb]
            [clojure.set :as set]
            [caesarhu.shun-tools.math-misc :as misc]))

(def squares (->> ["01", "04", "09", "16", "25", "36", "49", "64", "81"]
                  (map (fn [string]
                         (map misc/char-to-int (seq string))))
                  (map set)
                  (set)))
(def set-69 #{6 9})
(def necessary-digits #{0 1 2 3 4 5 6 8 9})

(defn expand-69
  [s]
  (let [s-set (set s)]
    (if (empty? (set/intersection s-set set-69))
      s-set
      (set/union s-set set-69))))

(defn all-necessary?
  [s1 s2]
  (set/superset? (expand-69 (concat s1 s2))
                 necessary-digits))

(defn cube-pair
  [s1 s2]
  (set (map set (apply comb/cartesian-product (map expand-69 [s1 s2])))))

(defn cube-pair-valid?
  [s1 s2]
  (and (all-necessary? s1 s2)
       (set/superset? (cube-pair s1 s2) squares)))

(defn euler-090
  []
  (count (set (for [s1 (comb/combinations (range 10) 6)
                    s2 (comb/combinations (range 10) 6)
                    :when (cube-pair-valid? s1 s2)]
                #{s1 s2}))))