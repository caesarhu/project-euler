(ns caesarhu.project-euler.euler-043
  (:require [caesarhu.shun-tools.math-misc :as misc]
            [caesarhu.shun-tools.or-tools.sat :as sat])
  (:import [com.google.ortools.sat CpModel IntVar CpSolverSolutionCallback LinearExpr]
           [com.google.ortools.util Domain]))

(defn add-digit-divisible
  [v p]
  (let [v-set (set v)]
    (for [i (range 10)
          :when (not (v-set i))
          :let [vi (cons i v)
                t (take 3 vi)]
          :when (zero? (mod (misc/to-number t) p))]
      vi)))

(defn filter-rules
  [primes]
  (let [init (map vector (range 10))]
    (reduce (fn [acc p]
              (mapcat #(add-digit-divisible % p) acc))
            init primes)))

(defn solve
  []
  (let [primes [1 17 13 11 7 5 3 2 1]]
    (->> (filter-rules primes)
         (map misc/to-number)
         (apply +))))

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