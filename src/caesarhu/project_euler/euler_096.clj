(ns caesarhu.project-euler.euler-096
  (:refer-clojure :exclude [==])
  (:require [clojure.string :as str]
            [clojure.core.logic :refer :all]
            [clojure.core.logic.fd :as fd]
            [caesarhu.shun-tools.math-misc :as misc]
            [rolling-stones.core :as sat :refer [! at-least at-most exactly]]))

(def sudoku-file "resources/data/p096_sudoku.txt")

(defn get-sudoku-puzzles
  [fname]
  (->> (str/split-lines (slurp fname))
       (partition 10)
       (map rest)
       (map (fn [ns] (mapcat #(map misc/char-to-int %) ns)))
       (map vec)))

(defn init-board [vars puzzle]
  (matche [vars puzzle]
          ([[] []]
           succeed)
          ([[_ . more-vars] [0 . more-puzzle]]
           (init-board more-vars more-puzzle))
          ([[num . more-vars] [num . more-puzzle]]
           (init-board more-vars more-puzzle))))

(defn solve-puzzle-logic [puzzle]
  (let [sdnum (fd/domain 1 2 3 4 5 6 7 8 9)
        board (repeatedly 81 lvar)
        rows (into [] (map vec (partition 9 board)))
        cols (apply map vector rows)

        get-square (fn [x y]
                     (for [x (range x (+ x 3)) y (range y (+ y 3))]
                       (get-in rows [x y])))

        squares (for [x (range 0 9 3) y (range 0 9 3)]
                  (get-square x y))]
    (run* [q]
          (== q board)
          (everyg #(fd/in % sdnum) board)
          (init-board board puzzle)
          (everyg fd/distinct rows)
          (everyg fd/distinct cols)
          (everyg fd/distinct squares))))

(defn solve-logic 
  []
  (->> (map solve-puzzle-logic (get-sudoku-puzzles sudoku-file))
       (map #(take 3 (first %)))
       (map #(apply str %))
       (map #(Integer/parseInt %))
       (apply +)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; SAT solution
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def box-base 3)
(def box-size (* box-base box-base))

(defn pos->number
  [x y z]
  (+ (* x box-size box-size)
     (* y box-size)
     z 1))

(defn number->z
  [n]
  (let [z (mod n box-size)]
    (if (zero? z) 9 z)))

(defn default-cnt
  []
  (let [rule-digit (for [x (range box-size)]
                     (for [y (range box-size)]
                       [(exactly 1 (map #(pos->number x y %) (range box-size)))
                        (exactly 1 (map #(pos->number x % y) (range box-size)))
                        (exactly 1 (map #(pos->number % y x) (range box-size)))]))
        rule-box (for [z (range box-size)]
                   (for [x (range 0 9 3) y (range 0 9 3)]
                     (exactly 1 (for [x (range x (+ x 3)) y (range y (+ y 3))]
                                  (pos->number x y z)))))]
    (flatten [rule-box rule-digit])))

(defn puzzle->cnf
  [puzzle]
  (->> (for [x (range box-size)]
         (for [y (range box-size)
               :let [z (puzzle (+ (* x box-size) y))]
               :when (pos-int? z)]
           (pos->number x y (dec z))))
       flatten
       (map vector)))

(defn solve-puzzle-sat
  [puzzle]
  (->> (sat/solve (concat (puzzle->cnf puzzle) (default-cnt)))
       (filter pos-int?)
       (map number->z)
       (partition 9)))

(defn solve-sat
  []
  (->> (map solve-puzzle-sat (get-sudoku-puzzles sudoku-file))
       (map #(take 3 (first %)))
       (map misc/to-number)
       (apply +)))

(comment
  (time (solve-sat))
  )