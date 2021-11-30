(ns caesarhu.project-euler.euler-092
  (:require [caesarhu.shun-tools.math-misc :as misc]
            [clojure.math.numeric-tower :as math]
            [clojure.core.cache.wrapped :as cache]
            [clojure.core.memoize :as memo]))

(defn square-digits-sum
  [n]
  (->> (map #(* % %) (misc/digits n))
       (apply +)))

(defn cycle-digit
  [n]
  (cond (zero? n) 0
        (= 1 n) 1
        (= 89 n) 89
        :else (recur (square-digits-sum n))))

(defn happy-number?
  [n]
  (= 1 (cycle-digit n)))

(defn brute-force
  [limit]
  (->> (range 1 limit)
       (map square-digits-sum)
       (filter (complement happy-number?))
       count))

(def count-happy-number
  (memo/fifo
   (fn [n k]
     (let [result (atom 0)]
       (cond
         (and (zero? n) (zero? k)) 1
         (neg? n) 0
         (and (pos? n) (zero? k)) 0
         :else (do
                 (doseq [i (range 10)
                         :let [x (- n (* i i))
                               y (dec k)]]
                   (swap! result +' (count-happy-number x y)))
                 @result))))
   :fifo/threshold 1000000))

  (defn solve
    [power]
    (let [limit (* 9 9 power)
          happy-numbers (filter happy-number? (range limit))]
      (->> (map #(count-happy-number % power) happy-numbers)
           (apply +')
           (- (math/expt 10 power) 1))))

(comment
  (time (solve 50))
  (time (brute-force 10000000))
  )