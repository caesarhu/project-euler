(ns caesarhu.project-euler.euler-074
  (:require [caesarhu.shun-tools.math-misc :as misc]))

(def fac-digits
  (->> (take 10 misc/factorial-seq)
       (map int)
       vec))

(def count-repeating-terms
  (memoize
    (fn [n]
      (let [known-loops { 1 1, 2 1, 145 1, 169 3, 871 2, 872 2, 40585 1}]
        (if-let [num-terms (get known-loops n)]
          num-terms
          (inc (count-repeating-terms (misc/sum-of fac-digits n))))))))

(defn solve [n num-terms]
  (->> (range 1 n)
       (filter #(= num-terms (count-repeating-terms %)))
       count))

; (time (solve 1000000 60))