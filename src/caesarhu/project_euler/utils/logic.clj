(ns caesarhu.project-euler.utils.logic
  (:refer-clojure :exclude [==])
  (:require [clojure.core.logic :refer :all]
            [clojure.core.logic.fd :as fd]))

(defne counto-helper
  [coll res]
  ([[] 0])
  ([[h . t] res] (fresh [x]
                        (counto-helper t x)
                        (fd/+ x 1 res))))

(defn counto
  "This logical function unifies result with the number
  of elements contained in lst."
  [lst result]
  (counto-helper lst result))

(comment
  (run* [q]
        (counto [1 2 3] q))
  )

(defn lasto
  [xs x]
  (fresh [y]
         (appendo y [x] xs)))

(defne ntho [l i f]
  ([[x . _] 0 f] (== f x))
  ([[x . xs] i f] (fresh [j] (fd/- i 1 j) (ntho xs j f)))
  ([[] i f] (fd/> i 0) (nilo f)))

(defn reduceo [g]
  (fn reduceo* [val coll ret]
    (conde
     [(== coll ()) (== val ret)]
     [(fresh [x xs ret']
             (conso x xs coll)
             (g val x ret')
             (reduceo* ret' xs ret))])))

(defn sumo [l sum]
  ((reduceo fd/+) 0 l sum))

(defn same-lengtho [xs ys]
  (conde
   [(== xs ()) (== ys ())]
   [(fresh [x xs' y ys']
           (conso x xs' xs)
           (conso y ys' ys)
           (same-lengtho xs' ys'))]))

(defn reverso* [l r]
  (conde
   [(== l ()) (== r ())]
   [(fresh [la ld ldr]
           (conso la ld l)
           (appendo ldr (list la) r)
           (reverso* ld ldr))]))

(defn reverso [l r]
  (all (same-lengtho l r)
       (reverso* l r)))

(defn numbero
  [l n]
  ((reduceo (fn [n d n']
              (fresh [x]
                     (fd/* n 10 x)
                     (fd/+ x d n'))))
   0 l n))

(defn modulo
  [l p]
  (fresh [x]
         (numbero l x)
         (fd/eq
          (= (* (/ x p) p) x))))

(comment
  (run* [q]
        (numbero [1 2 3] q))
  )
