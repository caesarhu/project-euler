(ns caesarhu.project-euler.euler-043-logic
  (:refer-clojure :exclude [==])
  (:require [clojure.core.logic :refer :all]
            [clojure.core.logic.fd :as fd]
            [caesarhu.project-euler.utils.logic :as l]
            [caesarhu.shun-tools.math-misc :as misc]))

(defne takeo
  [n coll res]
  ([0 _ []])
  ([n [] []]
   (fd/!= n 0))
  ([n [chead . ctail] [rhead . rtail]]
   (fresh [n']
          (fd/+ n' 1 n)
          (== chead rhead)
          (takeo n' ctail rtail))))

(defn mapo
  "core.logic version of map"
  [fo vs rs]
  (conda
   [(emptyo vs) (emptyo rs)]
   [(fresh [v r restvs restrs]
           (conso v restvs vs)
           (conso r restrs rs)
           (fo v r)
           (mapo fo restvs restrs))]))

(defn inco
  [n res]
  (project [n]
           (== (inc n) res)))

(defn add-digit
  [digits p res]
  (fresh [digit d3 num]
         (membero digit (range 10))
         (conso digit digits res)
         (distincto res)
         (takeo 3 res d3)
         (l/numbero d3 num)
         (fd/eq
          (= num (* (/ num p) p)))))

(comment
  (run* [q]
        (mapo inco (range 10) q))
)