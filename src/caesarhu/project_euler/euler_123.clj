(ns caesarhu.project-euler.euler-123
  (:require [caesarhu.shun-tools.primes :as p]
            [clojure.math.numeric-tower :as math]
            [caesarhu.shun-tools.math-misc :as misc]))

(defn power-mod
  [n p m]
  (loop [digits (misc/digits p 2)
         result 1]
    (let [f (first digits)
          d (mod (*' result result) m)]
      (cond
        (empty? digits) result
        (= 1 f) (recur (rest digits) (mod (*' d n) m))
        :else (recur (rest digits) d)))))

(defn prime-square-remainder
  [[n p]]
  (let [square (* p p)
        minus-mod (power-mod (dec p) n square)
        plus-mod (power-mod (inc p) n square)]
    (mod (+ minus-mod plus-mod) square)))

(def prime-seq
  (map #(vector %1 %2) (rest (range)) p/primes))

(defn euler-123
  [limit]
  (->> (drop-while #(<= (first %) 7307) prime-seq)
       (drop-while #(< (prime-square-remainder %) limit))
       first))

(comment
  (time (euler-123 (math/expt 10 10)))
  )