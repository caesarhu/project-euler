(ns caesarhu.project-euler.euler-185
  (:refer-clojure :exclude [==])
  (:require [caesarhu.shun-tools.math-misc :as misc]
            [clojure.core.logic :refer :all]
            [clojure.core.logic.fd :as fd]
            [rolling-stones.core :as sat :refer [! at-least at-most exactly]]))

(def target
  [[5616185650518293 2]
   [3847439647293047 1]
   [5855462940810587 3]
   [9742855507068353 3]
   [4296849643607543 3]
   [3174248439465858 1]
   [4513559094146117 2]
   [7890971548908067 3]
   [8157356344118483 1]
   [2615250744386899 2]
   [8690095851526254 3]
   [6375711915077050 1]
   [6913859173121360 1]
   [6442889055042768 2]
   [2321386104303845 0]
   [2326509471271448 2]
   [5251583379644322 2]
   [1748270476758276 3]
   [4895722652190306 1]
   [3041631117224635 3]
   [1841236454324589 3]
   [2659862637316867 2]])

(def sample
  [[90342 2]
   [70794 0]
   [39458 2]
   [34109 1]
   [51545 2]
   [12531 1]])

(def base 10)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; clojure core.logic solution, sample solved, but too slow to target
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn sumo [vars sum]
  (fresh [vhead vtail run-sum]
         (conde
          [(== vars ()) (== sum 0)]
          [(conso vhead vtail vars)
           (fd/+ vhead run-sum sum)
           (sumo vtail run-sum)])))

(defn number-mind
  [s]
  (let [length (count (misc/digits (ffirst s)))
        vars (repeatedly (* length base) lvar)
        matrix (->> vars (partition base) (map vec) (into []))
        rule-fd (fn [[digits n]]
                  (let [vs (->> (map-indexed vector (misc/digits digits))
                                (map #(get-in matrix %)))]
                    (sumo vs n)))]
    (run* [q]
          (== q matrix)
          (everyg #(fd/in % (fd/domain 0 1)) vars)
          (everyg #(sumo % 1) matrix)
          (everyg #(rule-fd %) s))))

(defn matrix->number
  [m]
  (->> (for [s m]
         (for [j (range base)
               :when (= (s j) 1)]
           j))
       (apply concat)
       (misc/to-number)))

(defn solve-logic
  [s]
  (-> s number-mind first matrix->number))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; SAT solution, using Rolling Stones https://github.com/Engelberg/rolling-stones
; Elapsed time: 664.560791 msecs, very fast!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn pos->number
  [x y]
  (+ (* x base) y 1))

(defn digits-cnf
  [length]
  (for [i (range length)]
    (exactly 1 (map #(pos->number i %) (range base)))))

(defn rule->cnf
  [[ds n :as rule]]
  (->> (map-indexed vector (misc/digits ds))
       (map #(apply pos->number %))
       (exactly n)))

(defn rules->cnf
  [rules]
  (let [length (count (misc/digits (ffirst rules)))]
    (concat (digits-cnf length)
            (map rule->cnf rules))))

(defn answer->number
  [s]
  (->> (filter pos-int? s)
       (map #(mod (dec %) base))
       (misc/to-number)))

(defn solve-sat
  [rules]
  (->> rules rules->cnf sat/solve answer->number))

(comment
  (time (solve-sat target))
  )