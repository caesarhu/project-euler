(ns caesarhu.project-euler.utils.logic
  (:refer-clojure :exclude [==])
  (:require [clojure.core.logic :refer :all]
            [clojure.core.logic.fd :as fd]))

(defn counto
  [coll count]
  (conde
   [(conda
     [(emptyo coll) (== count 0)]
     [(== count 0) (emptyo coll)])]
   [(fresh [next-count head tail]
           (fd/+ next-count 1 count)
           (conso head tail coll)
           (counto tail next-count))]))

(defn lasto
  [xs x]
  (fresh [y]
         (appendo y [x] xs)))

(defne ntho [l i f]
  ([[x . _] 0 f] (== f x))
  ([[x . xs] i f] (fresh [j] (fd/- i 1 j) (ntho xs j f)))
  ([[] i f] (fd/> i 0) (nilo f)))

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

(defn same-lengtho [xs ys]
  (conde
   [(== xs ()) (== ys ())]
   [(fresh [x xs' y ys']
           (conso x xs' xs)
           (conso y ys' ys)
           (same-lengtho xs' ys'))]))

(defn reverseo
  [xs ys]
  (letfn [(reverso* [l r]
                    (conde
                     [(== l ()) (== r ())]
                     [(fresh [la ld ldr]
                             (conso la ld l)
                             (appendo ldr (list la) r)
                             (reverso* ld ldr))]))]
    (all (same-lengtho xs ys)
         (reverso* xs ys))))

(defn reduceo [g]
  (fn reduceo* [acc coll ret]
    (conde
     [(emptyo coll) (== acc ret)]
     [(fresh [x xs ret']
             (conso x xs coll)
             (g acc x ret')
             (reduceo* ret' xs ret))])))

(defn sumo [l sum]
  ((reduceo fd/+) 0 l sum))

(defn numbero
  [l n]
  ((reduceo (fn [n d n']
              (fresh [x]
                     (fd/* n 10 x)
                     (fd/+ x d n'))))
   0 l n))

(defn maxo
  [l res]
  ((reduceo (fn [acc n next]
              (conda
               [(fd/> n acc) (== next n)]
               [(== next acc)])))
   Integer/MIN_VALUE l res))

(defn mino
  [l res]
  ((reduceo (fn [acc n next]
              (conda
               [(fd/< n acc) (== next n)]
               [(== next acc)])))
   Integer/MAX_VALUE l res))

(defne mapo
  [f coll target]
  ([_ [] []])
  ([_ [c . cs] [t . ts]]
   (f c t)
   (mapo f cs ts)))

(comment

  )