(ns caesarhu.project-euler.euler-163
  (:require [clojure.math.numeric-tower :as math]))

; https://oeis.org/A210687
; a(n) = (1678*n^3+3117*n^2+88*n-345*Mod[n,2]-320*Mod[n,3]-90*Mod[n,4]-288*Mod[n^3-n^2+n,5])/240. (Bill Daly)

(defn oeis-a210687
  [n]
  (quot (+ (* 1678 (math/expt n 3))
           (* 3117 (math/expt n 2))
           (* 88 n)
           (- (* 345 (mod n 2)))
           (- (* 320 (mod n 3)))
           (- (* 90 (mod n 4)))
           (- (* 288 (mod (+ (math/expt n 3) (- (math/expt n 2)) n) 5)))) 
        240))

; https://projecteuler.net/thread=163;page=5#last KnallkopPy's solution

(defn solve
  [n]
  (loop [index 3
         result-map {0 0 1 16 2 104}]
    (if (> index n)
      (result-map n)
      (let [value (-> (+ (* 3 (result-map (- index 1)))
                         (- (* 3 (result-map (- index 2))))
                         (result-map (- index 3))
                         6)
                      (#(if (zero? (mod index 2))
                          (+ % 25)
                          (+ % 12)))
                      (#(if (= 1 (mod index 3))
                          %
                          (+ % 12)))
                      (#(if (= 1 (mod index 4))
                          %
                          (+ % 3)))
                      (#(if (= 1 (mod index 5))
                          %
                          (if (or (= 2 (mod index 5))
                                  (zero? (mod index 5)))
                            (+　%　12)
                            (+　%　6)))))]
        (recur (inc index) (merge result-map {index value}))))))

(defn euler-163
  []
  (time (solve 36)))
