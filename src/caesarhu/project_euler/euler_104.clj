(ns caesarhu.project-euler.euler-104
  (:require [clojure.math.numeric-tower :as math]
            [caesarhu.shun-tools.math-misc :as misc]))

(defn pandigital?
  [n]
  (= "123456789" (apply str (sort (take 9 (str n))))))

(defn fib-index
  [index [i _ _ :as fib]]
  (let [fib-fn (fn [[i a b]]
                 [(inc i) b (+' a b)])]
    (if (= index i)
      fib
      (recur index (fib-fn fib)))))

(def fib-tail-seq
  (let [limit (math/expt 10 9)]
    (iterate (fn [[i a b]]
               [(inc i) b (mod (+ a b) limit)]) [0 1 0])))

(defn euler-104
  []
  (loop [pandigital-tail (filter #(pandigital? (last %)) fib-tail-seq)
         fib-init [0 1 0]]
    (let [fib-vec (fib-index (ffirst pandigital-tail) fib-init)]
      (if (pandigital? (last fib-vec))
        (first fib-vec)
        (recur (rest pandigital-tail) fib-vec)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def phi (/ (inc (math/sqrt 5)) 2))
(def log-phi (Math/log10 phi))
(def log-sqrt-5 (Math/log10 (math/sqrt 5)))

(defn fib-head
  [k]
  (let [log-k (+ (* k log-phi)
                 (- log-sqrt-5))
        log-9-digits (+ log-k (- (int log-k)) 8)]
    (int (math/expt 10 log-9-digits))))

(defn euler-104-fast
  []
  (first (for [[i _ _] (filter #(pandigital? (last %)) fib-tail-seq)
               :when (pandigital? (fib-head i))]
           i)))