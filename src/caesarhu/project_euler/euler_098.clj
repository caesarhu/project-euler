(ns caesarhu.project-euler.euler-098
  (:require [caesarhu.shun-tools.math-misc :as misc]
            [clojure.math.numeric-tower :as math]))

(defn strip-quotes [s]
  (apply str (filter #(not= % \") s)))

(defn get-data [fname]
  (map
    strip-quotes
    (clojure.string/split
      (slurp fname) #",")))

(defn anagrams [fname]
  (->> (get-data fname)
       (map #(hash-map (sort %) [%]))
       (apply merge-with concat)
       (filter #(> (count (val %)) 1))
       (map val)))

(defn squares-range [digits]
  (let [lower (math/expt 10 (dec digits))
        upper (* lower 10)]
    (->> misc/squares
         (drop-while #(< % lower))
         (take-while #(< % upper)))))

(defn create-mapping [word n]
  (zipmap (seq word) (misc/digits n)))

(defn apply-mapping [word mapping]
  (->> word
       (replace mapping)
       (apply str)
       (Integer/parseInt)))

(defn square-anagrams [[a b _]]
  (let [dig (count a)]
    (for [sq (squares-range dig)
          :let [mapping (create-mapping a sq)
                result  (apply-mapping b mapping)]
          :when (and
                  (misc/is-square? result)
                  (= (count (str sq)) (count (str result)))
                  (= dig (count (distinct (vals mapping)))))]
      result)))

(defn solve [fname]
  (->> (anagrams fname)
       (map square-anagrams)
       flatten
       (reduce max)))

(time (solve "resources/data/p098_words.txt"))
