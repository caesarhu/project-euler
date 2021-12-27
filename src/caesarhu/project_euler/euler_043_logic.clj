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
   (!= n 0))
  ([n [chead . ctail] [rhead . rtail]]
   (fresh [n']
          (fd/+ n' 1 n)
          (== chead rhead)
          (takeo n' ctail rtail))))

(defn add-digit
  [p digits res]
  (fresh [digit d3 num]
         (membero digit (range 10))
         (conso digit digits res)
         (distincto res)
         (takeo 3 res d3)
         (l/numbero d3 num)
         (fd/eq
          (= num (* (/ num p) p)))))

(defn get-solutions
  [primes]
  (run* [q]
        (fresh [d]
               (fd/in d (fd/interval 9))
               ((l/reduceo add-digit) [[d]] primes q))))

(comment
  (get-solutions [1])
  (run* [q]
        (fresh [d9 d8 d7 d6 d5 d4 d3 d2 d1]
               (fd/in d9 (fd/interval 9))
               (add-digit 1 [d9] d8)
               (add-digit 17 d8 d7)
               (add-digit 13 d7 d6)
               (add-digit 11 d6 d5)
               (add-digit 7 d5 d4)
               (add-digit 5 d4 d3)
               (add-digit 3 d3 d2)
               (add-digit 2 d2 d1)
               (add-digit 1 d1 q)))
)