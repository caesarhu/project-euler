(ns caesarhu.project-euler.euler-057
  (:require [caesarhu.shun-tools.math-misc :as misc]))

(def root-fraction-2
  (iterate (fn [[a b]]
             (let [new-a (+' a b)]
               [new-a (+' a new-a)])) [2 3]))

(defn solve
  [n]
  (->> (take n root-fraction-2)
       (map (fn [[a b]]
              (< (count (misc/digits a))
                 (count (misc/digits b)))))
       (filter true?)
       count))

(comment
  (take 100 root-fraction-2)
  (time (solve 1000))
  )