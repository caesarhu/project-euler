(ns caesarhu.project-euler.euler-043
  (:require [caesarhu.shun-tools.math-misc :as misc]
            [caesarhu.shun-tools.or-tools.sat :as sat])
  (:import [com.google.ortools.sat CpModel IntVar CpSolverSolutionCallback LinearExpr]
           [com.google.ortools.util Domain]))

(defn is-different?
  [v]
  (= (count v) (count (set v))))

(defn is-divisible?
  [v d]
  (-> (take-last 3 v) misc/to-number (mod d) zero?))

(def init (map vector (range 1 10)))
(def digit (range 10))
(def primes [1 1 2 3 5 7 11 13 17])

(defn prod-digit
  [s]
  (mapcat #(map (fn [d]
                  (concat % [d])) digit) 
          s))

(defn next-digit
  [s prime]
  (->> (prod-digit s)
       (filter is-different?)
       (filter #(is-divisible? % prime))))

(defn solve
  []
  (->> (reduce (fn [acc prime]
                 (next-digit acc prime))
               init primes)
       (map misc/to-number)
       (apply +)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn solve-sat
  []
  (let [model (sat/new-model)
        primes [2 3 5 7 11 13 17]
        create-domain (fn [p]
                        (let [array (->> (range)
                                         (map #(* p %))
                                         (take-while #(< % 1000))
                                         long-array)]
                          (Domain/fromValues array)))
        vars (->> (repeatedly 9 #(.newIntVar model 0 9 ""))
                  (cons (.newIntVar model 1 9 ""))
                  vec)]
    (.addAllDifferent model (into-array vars))
    (doseq [i (range 7)
            :let [sub-vars (subvec vars (inc i) (+ i 4))
                  p (primes i)]]
      (.addLinearExpressionInDomain model (LinearExpr/scalProd (into-array sub-vars) (long-array [100 10 1])) (create-domain p)))
    (->> (sat/solutions model (into-array vars))
         :values
         (map misc/to-number)
         (apply +))))

(comment
  (time (solve))
  (time (solve-sat))
  )