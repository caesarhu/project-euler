(ns caesarhu.project-euler.euler-018)

(def raw-euler-18-triangle
  [75
   95 64
   17 47 82
   18 35 87 10
   20 4 82 47 65
   19 1 23 75 3 34
   88 2 77 73 7 63 67
   99 65 4 28 6 16 70 92
   41 41 26 56 83 40 80 70 33
   41 48 72 33 47 32 37 16 94 29
   53 71 44 65 25 43 91 52 97 51 14
   70 11 33 28 77 73 17 78 39 68 17 57
   91 71 52 38 17 14 91 43 58 50 27 29 48
   63 66 4 68 89 53 67 30 73 16 69 87 40 31
   4 62 98 27 23 9 70 98 73 93 38 53 60 4 23])

(def euler-18-triangle
  (first (reduce (fn [[grid raw] n]
                   (let [[line other] (split-at n raw)]
                     [(conj grid (vec line)) other]))
                 [[] raw-euler-18-triangle] (range 1 16))))

(defn shorten-level
  [f level-1 level-2]
  (->> (partition 2 1 level-1)
       (map (fn [a [b c]]
              (apply f [(+ a b) (+ a c)])) level-2)))

(defn f-path
  [f coll]
  (loop [coll (reverse coll)]
    (if (= 1 (count coll))
      (ffirst coll)
      (recur (cons (shorten-level f (first coll) (second coll)) (drop 2 coll))))))

(defn brute-force
  []
  (f-path max euler-18-triangle))

(comment
  (time (brute-force))
  )

;; Nice job, "bibi"!
(defn merge-rows
  [a b]
  (map + (map #(apply max %) (partition 2 1 a)) b))

(defn solve
  [triangle]
  (reduce merge-rows (reverse triangle)))

(comment
  (time (solve euler-18-triangle))
  )
