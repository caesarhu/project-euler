(ns caesarhu.project-euler.euler-103
  (:require [clojure.math.combinatorics :as comb]
            [injest.core :as injest :refer [x> x>> => =>>]]
            [clojure.set :as set]))

(defn rule-bigger?
  [coll]
  (let [length (count coll)]
    (every? true? (for [i (range 2 length)
                        :let [j (dec i)
                              i-set (set (take i coll))
                              j-set (set (take-last j coll))]
                        :while (empty? (set/intersection i-set j-set))]
                    (> (apply + i-set) (apply + j-set))))))

(defn rule-sum?
  [coll]
  (->> (comb/subsets coll)
       (map #(apply + %))
       (apply distinct?)))

(defn special-sum-set?
  [coll]
  (and (rule-bigger? coll) (rule-sum? coll)))

(defn guess-next
  [coll]
  (let [a0 (nth coll (quot (count coll) 2))]
    (cons a0 (map #(+ a0 %) coll))))

(defn other-seq
  [length sum]
  (let [start (int (/ (- (/ (* sum 2) length) (dec length))
                      2))
        head (map #(+ start %) (range (dec length)))]
    (concat head [(- sum (apply + head))])))

(defn generate-seq
  [start length sum current-seq]
  (let [test-seq (concat current-seq (other-seq length sum))]
    (cond
      (> (+ (* start length) length (- 1)) sum) []
      (and (> (count current-seq) 1) 
           ((complement rule-bigger?) test-seq)) []
      (= 2 length) [(concat current-seq [start (- sum start)])]
      :else (let [new-sum (- sum start)
                  new-current-seq (conj current-seq start)]
              (apply concat (for [i (range (inc start) (inc new-sum))]
                              (generate-seq i (dec length) new-sum new-current-seq)))))))

(defn guess-answer
  [prev-answer]
  (let [guess (guess-next prev-answer)
        guess-sum (apply + guess)
        start (first guess)
        length (count guess)
        min-sum (- guess-sum 2)]
    (->> (iterate inc min-sum)
         (mapcat #(generate-seq start length % []))
         (some #(and (special-sum-set? %) %)))))

(defn solve
  [n]
  (if (= n 1)
    [1]
    (loop [result [1 2]]
      (if (= (count result) n)
        result
        (recur (guess-answer result))))))

(comment
  (generate-seq 39 8 567 [])
  (time (solve 7))
  )