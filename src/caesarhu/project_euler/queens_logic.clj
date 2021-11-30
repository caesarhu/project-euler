(ns caesarhu.project-euler.queens-logic
  (:refer-clojure :exclude [==])
  (:require [clojure.core.logic :refer :all]))

(defne safeo [q others]
  ([_ ()])
  ([[x1 y1] [[x2 y2] . t]]
   (!= x1 x2)
   (!= y1 y2)
   (project [x1 x2 y1 y2]
            (!= (- x2 x1) (- y2 y1))
            (!= (- x1 y2) (- x2 y1)))
   (safeo [x1 y1] t)))

(defne nqueenso [l n]
  ([() _])
  ([[[x y] . t] _]
   (nqueenso t n)
   (membero x (range n))
   (safeo [x y] t)))

(defn solve-nqueens [n]
  (run* [q]
        (== q (map vector (repeatedly lvar) (range n)))
        (nqueenso q n)))

(comment
  (time (count (solve-nqueens 10)))
  )