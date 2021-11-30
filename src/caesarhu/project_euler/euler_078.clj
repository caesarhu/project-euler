(ns caesarhu.project-euler.euler-078
  (:require [caesarhu.shun-tools.math-misc :as misc]))

(def sign (cycle [1 -1]))

(def pentagon-algorithm
  (memoize
    (fn [n]
      (cond
        (= n 0) 1
        (neg? n) 0
        :else (letfn [(term [k] (+ (pentagon-algorithm (- n (misc/pentagon k)))
                                   (pentagon-algorithm (- n (misc/pentagon (- k))))))]
                (mod
                  (reduce + (take-while (comp not zero?)
                                        (map #(* (term %2) %1) sign misc/integers)))
                  1000000))))))

(defn solve []
  (first
    (for [i misc/integers
          :when (zero? (pentagon-algorithm i))]
      i)))