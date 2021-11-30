(ns caesarhu.project-euler.euler-185-1
  (:require [caesarhu.shun-tools.or-tools.sat :as sat]
            [caesarhu.shun-tools.math-misc :as misc])
  (:import [com.google.ortools.sat CpModel IntVar CpSolverSolutionCallback LinearExpr]))

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

(defn pos->number
  [x y]
  (+ (* x base) y))

(defn init-model
  [length]
  (let [model (sat/new-model)
        vars (->> (map #(.newBoolVar model (str "v" %)) (range (* base length)))
                  vec)]
    (doseq [x (range length)
            :let [digits (->> (map #(pos->number x %) (range base))
                              (map vars))]]
      (.addEquality model (LinearExpr/booleanSum (into-array digits)) 1))
    {:model model :vars vars}))

(defn rule->constraint
  [m [ds n :as rule]]
  (let [{:keys [model vars]} m
        digits (->> (map-indexed vector (misc/digits ds))
                    (map #(apply pos->number %))
                    (map vars))]
    (.addEquality model (LinearExpr/booleanSum (into-array digits)) n)))

(defn rules->model
  [rules]
  (let [length (count (misc/digits (ffirst rules)))
        m (init-model length)]
    (doseq [rule rules]
      (rule->constraint m rule))
    m))

(defn answer->number
  [answer]
  (->> (map-indexed vector answer)
       (remove #(zero? (last %)))
       (map first)
       (map #(mod % base))
       (misc/to-number)))

(defn solve
  [rules]
  (->> (rules->model rules)
       vals
       (apply sat/solve)
       :values
       answer->number))

(comment
  (time (solve target))
  )