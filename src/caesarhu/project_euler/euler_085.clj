(ns caesarhu.project-euler.euler-085)

(defn num-rects [m n]
  (* (/ (+ (* m m) m) 2)
     (/ (+ (* n n) n) 2)))

(defn smallest-second [coll]
  (reduce #(min-key second %1 %2) coll))

(defn euler-85 [goal]
  (first
    (smallest-second
       (for [i (take-while #(< (num-rects % 1) goal) (rest (range)))
             j (take-while #(< 0 (- goal (num-rects i %))) (rest (range)))]
           [(* i j) (- goal (num-rects i j))]))))

; (time (euler-85 2000000))