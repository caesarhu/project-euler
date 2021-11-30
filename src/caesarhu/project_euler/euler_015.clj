(ns caesarhu.project-euler.euler-015)

(defn valid-fn
  [n]
  (fn [[i j]]
    (and (> n i -1)
         (> n j -1))))

(def directions [[1 0] [0 1]])

(defn move
  [[i j] [x y]]
  [(+ i x) (+ j y)])

(defn find-paths
  [n pos]
  (let [valid? (valid-fn n)]
    (->> (map #(move pos %) directions)
         (filter valid?))))

(defn walk
  [n]
  (let [join-paths (fn [coll-pos]
                     (if-let [paths (not-empty (find-paths n (last coll-pos)))]
                       (map #(conj coll-pos %) paths)
                       coll-pos))]
    (loop [paths [[[0 0]]]]
      (if (empty? (find-paths n (->> paths first last)))
        paths
        (recur (apply concat (for [path paths]
                               (join-paths path))))))))

(def walk-count
  (memoize
    (fn [n pos]
      (if (empty? (find-paths n pos))
        1
        (apply + (map #(walk-count n %) (find-paths n pos)))))))

(defn brute-force
  [n]
  (walk-count (inc n) [0 0]))

(defn solve
  [n]
  (reduce (fn [acc i]
            (/ (* acc (+ n i)) i))
          1 (range 1 (inc n))))

(comment
  (time (brute-force 20))
  (time (solve 20))
  )