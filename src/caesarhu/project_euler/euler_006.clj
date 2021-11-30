(ns caesarhu.project-euler.euler-006)

(defn square
  [n]
  (* n n))

(defn solve
  [n]
  (let [sum (apply + (range 1 (inc n)))
        sum-of-square (->> (map square (range 1 (inc n)))
                           (apply +))]
    (- (square sum) sum-of-square)))

; (time (solve 100))