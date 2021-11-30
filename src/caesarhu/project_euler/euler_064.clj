(ns caesarhu.project-euler.euler-064
  (:require [clojure.math.numeric-tower :as math]
            [caesarhu.shun-tools.math-misc :as misc]))

(defn square [x] (* x x))

(defn solve []
  (->> (range 2 (inc 10000))
       (remove misc/is-square?)
       (map misc/expand-continued-fraction)
       (filter #(even? (count %)))
       count))