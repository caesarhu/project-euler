(ns caesarhu.project-euler.euler-165
  (:require [clojure.math.combinatorics :as comb]))

(def blum-start [290797 0])

(defn next-blum-random
  [[s _]]
  (let [next-s (mod (* s s) 50515093)]
    [next-s (mod next-s 500)]))

(def L1 [[27, 44] [12, 32]])
(def L2 [[46, 53] [17, 62]])
(def L3 [[46, 70] [22, 40]])

(def lines
  (->> (iterate next-blum-random (next-blum-random blum-start))
       (take 20000)
       (map last)
       (partition 2)
       (partition 2)))

(defn formula
  [[[x1 y1] [x2 y2]]]
  (let [a (- y2 y1)
        b (- x1 x2)
        c (+ (* a x1) (* b y1))
        result [a b c]]
    (if (neg? a)
      (vec (map - result))
      result)))

(defn intersection
  [f-a f-b]
  (let [[a b c] f-a
        [d e f] f-b
        den (- (* a e) (* b d))]
    (when (not (zero? den))
      [(/ (- (* c e) (* b f)) den)
       (/ (- (* a f) (* c d)) den)])))

(defn between?
  [pair x]
  (let [[a b] (sort pair)]
    (and (> x a)
         (< x b))))

(defn line-inner?
  [line [x y]]
  (let [x-range (map first line)
        y-range (map last line)
        x? (or (apply = x x-range) (between? x-range x))
        y? (or (apply = y y-range) (between? y-range y))]
    (and x? y?)))

(defn true-intersection
  [line-a line-b]
  (let [i (intersection (formula line-a) (formula line-b))]
    (if i
      (and (line-inner? line-a i)
           (line-inner? line-b i)
           i)
      nil)))

(defn set-intersection
  [line other-lines]
  (set (for [other other-lines
             :let [i (true-intersection line other)]
             :when i]
         i)))

(defn brute-force
  []
  (loop [lines lines
         result #{}]
    (if (<= (count lines) 1)
      (count result)
      (recur (rest lines) (clojure.set/union result (set-intersection (first lines) (rest lines)))))))

(defn solve
  []
  (let [all-lines (map #(vector (vec (sort (map first %))) %) lines)
        group-zero (group-by #(apply = (first %)) all-lines)
        zero-x (get group-zero true)
        other-x (get group-zero false)
        other-x-lines (map last other-x)]
    (loop [result (apply clojure.set/union (for [line (map last zero-x)]
                                             (set-intersection line other-x-lines)))
           [current & others] (sort-by first other-x)]
      (if (empty? others)
        (count result)
        (let [[[_ x2] line] current
              other-lines (->> (take-while #(< (ffirst %) x2) others)
                               (map last))
              current-set (set-intersection line other-lines)]
          (recur (clojure.set/union result current-set) others))))))

