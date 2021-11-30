(ns caesarhu.project-euler.sudoku
  (:require [caesarhu.shun-tools.or-tools.sat :as sat])
  (:import [com.google.ortools.sat CpModel IntVar CpSolverSolutionCallback LinearExpr]
           [com.google.ortools.util Domain]))

(def base 3)
(def size (* base base))

(def target
  [[0, 6, 0, 0, 5, 0, 0, 2, 0]
   [0, 0, 0, 3, 0, 0, 0, 9, 0]
   [7, 0, 0, 6, 0, 0, 0, 1, 0]
   [0, 0, 6, 0, 3, 0, 4, 0, 0]
   [0, 0, 4, 0, 7, 0, 1, 0, 0]
   [0, 0, 5, 0, 9, 0, 8, 0, 0]
   [0, 4, 0, 0, 0, 1, 0, 0, 6]
   [0, 3, 0, 0, 0, 8, 0, 0, 0]
   [0, 2, 0, 0, 4, 0, 0, 5, 0]])

(def evil-1
  [[7 0 0 4 0 2 5 0 0]
   [0 0 0 0 0 8 1 4 3]
   [0 0 0 0 9 0 0 0 0]
   [0 0 0 0 0 0 0 7 6]
   [4 0 0 8 0 5 0 0 2]
   [2 8 0 0 0 0 0 0 0]
   [0 0 0 0 4 0 0 0 0]
   [8 4 5 6 0 0 0 0 0]
   [0 0 9 2 0 7 0 0 5]])

(defn boxs
  [board]
  (for [x (range 0 9 3) y (range 0 9 3)]
    (for [xx (range x (+ x 3)) yy (range y (+ y 3))]
      (get-in board [xx yy]))))

(defn sudoku-rules
  [puzzle]
  (let [model (sat/new-model)
        board (vec (for [i (range size)]
                     (vec (for [j (range size)]
                            (.newIntVar model 1 9 (str "v" (+ (* i size) j)))))))
        vars (for [i (range size)
                   j (range size)]
               (get-in board [i j]))]
    (doseq [i (range size)]
      (.addAllDifferent model (into-array (board i))) ; row rule
      (.addAllDifferent model (into-array (map #(get-in board [% i]) (range size))))) ; column rule
    (doseq [box (boxs board)]
      (.addAllDifferent model (into-array box))) ; box rule
    (doseq [i (range size)
            j (range size)
            :let [v (get-in puzzle [i j])]
            :when (pos-int? v)]
      (let [var (get-in board [i j])]
        (.addEquality model (LinearExpr/term var 1) v)))
    {:model model :board board :vars vars}))

(defn solve
  [puzzle]
  (let [{:keys [model board vars]} (sudoku-rules puzzle)]
    (->> (sat/solve model vars)
         :values
         (partition 9)
         vec
         clojure.pprint/pprint)))

(comment
  (time (solve evil-1))
  )