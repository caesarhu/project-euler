(ns caesarhu.project-euler.euler-154-try
  (:require [caesarhu.shun-tools.math-misc :as misc]))

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

(defn solve-old
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def target 200000)
(def default-power-map
  (generate-power-map target))

(defn get-power
  [p n]
  ((default-power-map p) n))

(defn coefficient-prime-power
  [n i p]
  (let [m (- n i)]
    (->> (map #(get-power p %) [n i m])
         (apply -))))

(defn coefficient-2-5
  [n bound-2 bound-5]
  (let [legal-5 (->> (range (inc n))
                     (filter #(>= (coefficient-prime-power n % 5) bound-5)))]
    (cond
      (and (zero? bound-5) (zero? bound-2)) (inc n)
      (zero? bound-2) (count legal-5)
      :else (->> (filter #(>= (coefficient-prime-power n % 2) bound-2) legal-5)
                 count))))

(defn brute-force
  [limit power-of-10]
  (let [max-power-5 (dec (count (misc/digits limit 5)))]
    (->> (for [i (range (inc limit))]
           (let [p2 (coefficient-prime-power limit i 2)
                 p5 (coefficient-prime-power limit i 5)
                 bound-2 (max 0 (- power-of-10 p2))
                 bound-5 (max 0 (- power-of-10 p5))]
             (if (> bound-5 max-power-5) 0
                 (coefficient-2-5 (- limit i) bound-2 bound-5))))
         (apply +))))

(comment
  (time (solve-old 2000 2))
  )