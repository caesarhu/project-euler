(ns caesarhu.project-euler.euler-112
  (:require [caesarhu.shun-tools.math-misc :as misc]))

(defn bouncy?
  [n]
  (let [digits (misc/digits n)]
    (not (or (< n 100)
             (apply >= digits)
             (apply <= digits)))))

(def bouncy-seq
  (iterate (fn [[i bool c proportion]]
             (let [n (inc i)
                   new-bool (bouncy? n)
                   new-c (if new-bool (inc c) c)]
               [n new-bool new-c (/ new-c n)]))
           [1 (bouncy? 1) 0 (/ 0 1)]))

(defn find-bouncy-proportion
  [target]
  (some #(and (= (last %) target) %) bouncy-seq))