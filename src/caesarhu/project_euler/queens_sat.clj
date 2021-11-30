(ns caesarhu.project-euler.queens-sat
  (:require [caesarhu.shun-tools.or-tools.sat :as sat]
            [clojure.math.combinatorics :as combo])
  (:import [com.google.ortools.sat CpModel CpSolver CpSolverStatus IntVar CpSolverSolutionCallback LinearExpr]))

(defn queens-clojure
  [n]
  (filter (fn [x] (every? #(apply distinct? (map-indexed % x)) [+ -]))
          (combo/permutations (range 1 (inc n)))))

(defn linear-distinct
  [model linear-expr-seq]
  (let [add-different (fn [left right]
                        (.addDifferent model left right))]
    (doseq [pair (combo/combinations linear-expr-seq 2)]
      (apply add-different pair))))

(defn solve-queens
  [board-size]
  (let [model (sat/new-model)
        queens (mapv #(.newIntVar model 0 (dec board-size) (str "x" %)) (range board-size))
        diag1 (for [i (range board-size)]
                (LinearExpr/affine (queens i) 1 i))
        diag2 (for [i (range board-size)]
                (LinearExpr/affine (queens i) 1 (- i)))]
    (.addAllDifferent model (into-array queens))
    diag1))

(comment
  (time (solve-queens 8))
  (time (count (queens-clojure 8)))
  )