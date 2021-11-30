(ns caesarhu.project-euler.euler-086
  (:require [clojure.math.numeric-tower :as math]))

(defn square? [x] (= x (math/expt (int (math/sqrt x)) 2)))

(defn cuboids [m]
  (reduce
    +
    (for [a (filter #(square? (+ (* % %) (* m m)))
                    (range 1 (inc (* 2 m))))]
      (if (> a m)
        (- (quot a 2) (- a m 1))
        (quot a 2)))))

(defn euler-86 [L]
  (count (take-while #(> L %) (reductions + (map cuboids (range))))))

; (time (euler-86 1000000))