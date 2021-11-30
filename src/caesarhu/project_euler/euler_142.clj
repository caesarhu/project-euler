(ns caesarhu.project-euler.euler-142
  (:require [caesarhu.shun-tools.primes :as p]
            [caesarhu.shun-tools.math-misc :as misc]
            [clojure.math.numeric-tower :as math]))

(defn find-square
  [limit]
  (first (for [n3 (range 3 limit)
               n4 (range 2 n3)
               :let [z (/ (- (* n3 n3) (* n4 n4)) 2)]
               :when (integer? z)
               n2 (range 1 n4)
               :let [n1 (+ (* n3 n3) (* n4 n4) (- (* n2 n2)))]
               :when (misc/is-square? n1)
               :let [x (/ (+ n1 (* n2 n2)) 2)
                     y (/ (- n1 (* n2 n2)) 2)
                     m (- y z)
                     n (+ y z)]
               :when (and (integer? x)
                          (integer? y)
                          (< z y)
                          (misc/is-square? m)
                          (misc/is-square? n))]
           [x y z])))

(defn start-number
  ([n]
   (start-number n 1))
  ([n start]
   (if (even? (+ n start))
     start
     (inc start))))

(defn get-square
  [limit]
  (for [A (range 3 limit)
        B (range (start-number A (int (/ A (math/sqrt 2)))) A 2)
        :let [A2 (* A A)
              B2 (* B B)
              A2-B2 (- A2 B2)]
        :when (misc/is-square? A2-B2)
        C (range (start-number B (math/sqrt A2-B2)) B 2)
        :let [C2 (* C C)
              x (/ (+ B2 C2) 2)
              y (/ (- (* 2 A2) B2 C2) 2)
              z (/ (- B2 C2) 2)]
        :when (and (misc/is-square? (- A2 C2))
                   (misc/is-square? (- (+ B2 C2) A2))
                   (> x y z))]
    [x y z]))

(defn euler-142
  []
  (apply + (first (get-square 1000))))