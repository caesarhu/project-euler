(ns caesarhu.project-euler.euler-055
  (:require [caesarhu.shun-tools.math-misc :as misc]))

(defn palindrome-number?
  [n]
  (= (str n)
     (apply str (reverse (str n)))))

(defn reverse-number
  [n]
  (->> (misc/digits n)
       (reverse)
       (apply str)
       (bigint)))

(defn reverse-sum
  [n]
  (drop 1 (iterate #(+ % (reverse-number %)) (bigint n))))

(def lychrel-limit 50)

(defn lychrel-number?
  [n]
  (->> (take lychrel-limit (reverse-sum n))
       (some palindrome-number?)
       not))

(defn solve
  [limit]
  (->> (range 1 (inc limit))
       (filter lychrel-number?)
       count))

; (time (solve 10000))