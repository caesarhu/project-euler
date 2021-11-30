(ns caesarhu.project-euler.euler-031)

(def coins [200 100 50 20 10 5 2 1])

(defn change[c v]
  (if (= (count c) 1)
    1
    (let [f (first c)]
      (reduce +
              (for [n (range 0 (inc (quot v f)))]
                (change (rest c) (- v (* n f))))))))

(defn solve []
  (change coins 200))

; (time (solve))

(defn change[c v]
  (let [f (first c)]
    (if (= f 1)
      1
      (reduce + (for [n (range 0 (inc (quot v f)))]
                  (change (rest c) (- v (* n f))))))))

(defn euler-31-bibi []
  (change coins 200))