(ns caesarhu.project-euler.euler-135)

(defn euler-135
  [limit]
  (->> (for [x (range 1 (inc limit))
             k (range 1 (min (dec (* 3 x)) (/ limit x)))
             :when (zero? (mod (+ x k) 4))]
         {(* x k) 1})
       (apply merge-with +)
       (filter #(= 10 (val %)))
       count))

