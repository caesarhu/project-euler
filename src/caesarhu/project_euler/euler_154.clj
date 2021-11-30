(ns caesarhu.project-euler.euler-154)

(defn generate-power-map
  [^long limit]
  (let [f2 (atom [0])
        f5 (atom [0])]
    (doseq [i (range 1 (inc limit))
            :let [i2 (quot i 2)
                  i5 (quot i 5)
                  n2 (@f2 i2)
                  n5 (@f5 i5)]]
      (swap! f2 assoc i (+ n2 i2))
      (swap! f5 assoc i (+ n5 i5)))
    {2 @f2 5 @f5}))

(defn solve
  [^long limit ^long power-of-10]
  (let [power-map (generate-power-map limit)
        get-power (fn [^long p ^long n] ((get power-map p) n))
        prime-power (fn [[^long i ^long j ^long k]]
                      [(apply - (map (partial get-power 2) [limit i j k]))
                       (apply - (map (partial get-power 5) [limit i j k]))])
        valid? (fn [coll] (every? #(>= % power-of-10) coll))
        ->count (fn [[^long i ^long j ^long k]] (cond
                                (= i j k) 1
                                (or (= i j) (= j k) (= i k)) 3
                                :else 6))]
    (reduce + (for [^long i (range (inc (quot limit 3)))
                    ^long j (range i (inc (quot (- limit i) 2)))
                    :let [k (- limit i j)]
                    :when (valid? (prime-power [i j k]))]
                (->count [i j k])))))

(defn euler-154
  []
  (solve 200000 12))

; (time (euler-154))
