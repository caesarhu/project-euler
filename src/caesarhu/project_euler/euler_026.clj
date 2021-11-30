(ns caesarhu.project-euler.euler-026
  (:require [caesarhu.shun-tools.floyd-cycle :as floyd]))

(defn div
  [[r q] d]
  (when (and r (not (zero? r)))
    [(mod (* 10 r) d) (quot (* 10 r) d)]))

(defn fdiv
  [d]
  #(div % d))

(defn decimal-digits
  [n]
  (let [decimal-seq (take-while some? (rest (iterate (fdiv n) [1 0])))
        cycle (floyd/floyd-cycle (fdiv n) [1 0])]
    (if cycle
      (let [starter (first cycle)
            head (->> (take-while #(not= starter %) decimal-seq)
                      (map last))
            tail (map last cycle)]
        {:head head :tail tail})
      {:head (map last decimal-seq)})))

(defn solve
  [limit]
  (apply max-key #(count (floyd/floyd-cycle (fdiv %) [1 0])) (range 1 limit)))

(defn brent
  [f init-state]
  (loop [tortoise init-state
         hare init-state
         step 1
         power 1]
    (let [next-hare (f hare)]
      (cond 
        (or (nil? next-hare) (= next-hare tortoise)) [step next-hare]
        (= step power) (recur next-hare next-hare 1 (* 2 power))
        :else (recur tortoise next-hare (inc step) power)))))

(defn solve2
  [limit]
  (->> (range 1 limit)
       (map #(vector % (let [v (brent (fdiv %) [1 0])]
                         (if (last v) (first v) 0))))
       (apply max-key last)
       first))

(comment
  (brent (fdiv 1) [1 0])
  (time (solve2 10000))
  (time (solve 10000))
  "循環小數的判斷，應用Floyd Cycle Detection Algorithm，又稱為龜免賽跑算法"
  )