(ns caesarhu.project-euler.euler-008)

(defn char->number
  [char]
  (- (int char) 48))

(def digits-1000
  (->> (slurp "resources/data/1000-digit-number.txt")
       (apply str)
       (seq)
       (map char->number)
       (remove neg?)))

(defn solve
  [n]
  (->> digits-1000
       (partition n 1)
       (map #(apply * %))
       (apply max)))

(comment
  (time (solve 13))
  )
