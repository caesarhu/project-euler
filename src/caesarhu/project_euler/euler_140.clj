(ns caesarhu.project-euler.euler-140)

(defn G
  [[a b c d]]
  (let [e (- (* 7 (inc c)) a)]
    [b c d e]))

(def G-seq
  (concat [2] (map last (iterate G [0 0 2 5]))))

(defn euler-140
  [limit]
  (->> (take limit G-seq)
       (apply +)))

; (time (euler-140 30))