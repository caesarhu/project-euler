(ns caesarhu.project-euler.euler-014)

(defn collatz-length
  [n]
  (loop [n n
         counter 0]
    (cond
      (= n 1) counter
      (even? n) (recur (/ n 2) (inc counter))
      :else (recur (inc (* n 3)) (inc counter)))))

(def collatz-seq
  (iterate (fn [[a b]]
             (let [start (inc a)]
               [start (collatz-length start)]))
           [1 1]))

(defn solve
  [limit]
  (->> (take-while #(< (first %) limit) collatz-seq)
       (apply max-key last)
       first))

(comment
  (time (solve 1000000))
  )
