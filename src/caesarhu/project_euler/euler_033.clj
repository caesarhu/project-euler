(ns caesarhu.project-euler.euler-033
  (:require [caesarhu.shun-tools.math-misc :as misc]))

(defn curious-fraction? [n d]
  (let [ns (misc/digits n)
        ds (misc/digits d)]
    (and
      (pos? (second ds))
      (= (second ns) (first ds))
      (= (/ n d) (/ (first ns) (second ds))))))

(defn solve [n]
  (denominator (reduce *
                       (for [a (range 10 n)
                             b (range (inc a) n)
                             :when (curious-fraction? a b)]
                         (/ a b)))))

(comment
  (time (solve 100))
  )