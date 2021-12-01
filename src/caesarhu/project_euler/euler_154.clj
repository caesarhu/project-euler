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
  [limit power-of-10]
  (let [power-map (generate-power-map limit)
        get-power (fn [p n] ((get power-map p) n))
        count-power (fn [[i j k]]
                      (let [p2 (apply - (map (partial get-power 2) [limit i j k]))
                            p5 (apply - (map (partial get-power 5) [limit i j k]))]
                        (if (and (>= p5 power-of-10) (>= p2 power-of-10))
                          (cond
                            ; (= i j k) 1
                            (or (= j k) (= i k) (= i j)) 3
                            :else 6)
                          0)))
        result (atom 0)]
    (doseq [i (range (inc (quot limit 3)))
            j (range i (inc (quot (- limit i) 2)))
            :let [k (- limit i j)]]
      (swap! result + (count-power [i j k])))
    @result))

(defn solve2
  [limit]
  (let [result (atom 0)]
    (doseq [i (range (inc (quot limit 3)))
            j (range i (inc (quot (- limit i) 2)))]
      (swap! result inc))
    @result))

(defn solve3
  [limit]
  (->> (range (inc (quot limit 3)))
       (map #(range % (inc (quot (- limit %) 2))))
       (map count)
       (apply +)))

(comment
  (time (solve3 20000))
  (time (solve 20000 8))
  )

