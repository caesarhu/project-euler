(ns caesarhu.project-euler.euler-066
  (:require [clojure.math.numeric-tower :as math]
            [caesarhu.shun-tools.math-misc :as misc]
            [caesarhu.shun-tools.pell-equation :as pell]))

(defn find-fundamental-solution
  "find the fundamental solution of pell's equation,  x^2 - dy^2 = 1"
  [d]
  (let [cf (misc/expand-continued-fraction d)
        as (lazy-cat cf (cycle (rest cf)))]
    (loop [h2 0, h1 1,
           k2 1, k1 0,
           as as, n 0]
      (if (and (>= n 1) (= 1 (-' (*' h1 h1) (*' d k1 k1))))
        [h1 k1]
        (recur h1 (+' (*' (first as) h1) h2)
               k1 (+' (*' (first as) k1) k2)
               (rest as) (inc n))))))

(defn solve []
  (->> (range 2 (inc 1000))
       (remove misc/is-square?)
       (map (fn [d] [d (first (find-fundamental-solution d))]))
       (apply (partial max-key second))))

(defn chakravala-method
  [D]
  (loop [a (first (math/exact-integer-sqrt D))
         b 1
         k (- (* a a) D)]
    (if (= k 1)
      a
      (let [m (apply min-key #(math/abs (- (* % %) D))
                     (filter #(integer? (/ (+ a (* % b)) k))
                             (range (* 2 (first (math/exact-integer-sqrt D))))))]
        (recur (bigint (/ (+ (* a m) (* D b)) (math/abs k)))
               (/ (+ a (* b m)) (math/abs k))
               (/ (- (* m m ) D) k))))))

(defn euler-066
  [limit]
  (->> (range 2 (inc limit))
       (remove misc/is-square?)
       (map chakravala-method)
       (apply max)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; require pell-equation

(defn solve-pell
  [limit]
  (->> (range 2 (inc limit))
       (remove misc/is-square?)
       (map #(first (pell/pell-solutions % 1)))
       (map first)
       (apply max)))

(comment
  (time (solve-pell 1000))
  (time (solve))
  )