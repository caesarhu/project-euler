(ns caesarhu.project-euler.euler-149)

(def length 10)

(defn max-subseq-sum
  [coll]
  (first (reduce (fn [[max-so-far max-ending-here] v]
                   (let [here (max 0 (+ max-ending-here v))
                         so-far (max here max-so-far)]
                     [so-far here]))
                 [0 0] coll)))

(defn pseudo
  [x]
  (- (mod x 1000000) 500000))

(defn small-random
  [k]
  (pseudo (+ 100003 (- (* 200003 k)) (* 300007 k k k))))

(defn pseudo-vector
  [limit]
  (let [v (atom (vec (repeat limit 0)))
        _ (doseq [i (range 1 (inc limit))]
            (if (<= i 55)
              (swap! v assoc (dec i) (small-random i))
              (let [result (pseudo (+ (nth @v (- i 24 1))
                                      (nth @v (- i 55 1))
                                      1000000))]
                (swap! v assoc (dec i) result))))]
    @v))

(def pseudo-vec
  (pseudo-vector (* length length)))

(defn pos-value
  [[i j]]
  (nth pseudo-vec (+ (* i length) j)))

(defn make-line
  [[i j]]
  (let [horizontal (for [y (range length)]
                     (pos-value [i y]))
        vertical (for [x (range length)]
                   (pos-value [x j]))]
    [horizontal vertical]))

(defn euler-149
  []
  (reduce (fn [acc pos]
            (apply max acc (map max-subseq-sum (make-line pos))))
          0 (for [i (range length)]
              [i i])))

