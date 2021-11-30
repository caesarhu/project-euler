(ns caesarhu.project-euler.euler-028
  (:require [caesarhu.shun-tools.math-misc :as misc]))

(def spiral-seq
  (mapcat #(repeat 4 %) (map #(* % 2) misc/integers)))

(defn accumulate [coll n]
  (concat coll [(+ (last coll) n)]))

(defn solve [n]
  (reduce + (reduce accumulate (list 1) (take-while #(<= % n) spiral-seq))))

; (time (solve 1001))

(defn euler-28-revised [n]
  "Compute the sum of the diagonals in an n by n clockwise spiral. "
  (assert (and (> n 0) (odd? n))) ;; Must be odd to have diagonals!
  (let [diags (fn [m] (take 4 (iterate #(- % (dec m)) (* m m))))
        squares (take-while #(> % 1) (iterate #(- % 2) n))]
    (reduce + (flatten [(map diags squares) 1]))))