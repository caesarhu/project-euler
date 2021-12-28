(ns caesarhu.project-euler.euler-043-logic
  (:refer-clojure :exclude [==])
  (:require [clojure.core.logic :refer :all]
            [clojure.core.logic.fd :as fd]
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

(defn reduceo [g]
  (fn reduceo* [val coll ret]
    (conda
     [(emptyo coll) (== val ret)]
     [(fresh [x xs ret']
             (conso x xs coll)
             (g x val ret')
             (reduceo* ret' xs ret))])))

(defn numbero
  [l n]
  ((reduceo (fn [d n n']
              (fresh [x]
                     (fd/* n 10 x)
                     (fd/+ x d n'))))
   0 l n))

(defn add-digit
  [p digits res]
  (fresh [digit d3 num]
         (membero digit (range 10))
         (conso digit digits res)
         (distincto res)
         (takeo 3 res d3)
         (numbero d3 num)
         (fd/eq
          (= num (* (/ num p) p)))))

(defn prime-digits
  [init primes res]
  ((reduceo add-digit) init primes res))

(defn solve-logic
  []
  (let [primes [1 17 13 11 7 5 3 2 1]]
    (->> (run* [q]
               (fresh [d]
                      (membero d (range 10))
                      (prime-digits [d] primes q)))
         (map misc/to-number)
         (apply +))))

(comment
  (time (solve-logic))
)