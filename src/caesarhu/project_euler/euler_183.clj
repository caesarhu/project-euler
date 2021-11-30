(ns caesarhu.project-euler.euler-183
  (:require [clojure.math.numeric-tower :as math]
            [caesarhu.shun-tools.primes :as p]))

(defn quot-recur
  [n p]
  (loop [n n]
    (if (not (zero? (mod n p)))
      n
      (recur (quot n p)))))

(defn terminating?
  [r]
  (if (== (long r) r)
    true
    (let [d (denominator (rationalize r))]
      (-> (quot-recur d 2)
          (quot-recur 5)
          (= 1)))))

(defn calc-log
  [n k]
  (* k (Math/log (/ n k))))

(defn max-parts
  [n]
  (let [k (->> (range 2 n)
               (apply max-key #(calc-log n %)))]
    (if (terminating? (/ n k))
      [(- n) k]
      [n k])))

(defn solve-slow
  [limit]
  (->> (range 5 (inc limit))
       (map max-parts)
       (map first)
       (apply +)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 猜測比值為自然對數的倒數
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def guess-ratio (/ (Math/E)))

(defn guess-number
  [n]
  (let [x (long (* n guess-ratio))
        k (apply max-key #(calc-log n %) [x (inc x)])]
    (if (terminating? (/ n k))
      [(- n) k]
      [n k])))

(defn solve
  [limit]
  (->> (range 5 (inc limit))
       (map guess-number)
       (map first)
       (apply +)))

(comment
  (time (max-parts 100000000))
  (time (solve 10000))
  )