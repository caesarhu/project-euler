(ns caesarhu.project-euler.euler-161
  (:require [clojure.math.numeric-tower :as math]))

(def triominoes
  [[[0 0] [0 1] [0 2]]    ; ---
   [[0 0] [1 0] [2 0]]    ; |
   [[0 0] [0 1] [1 1]]    ; ┐
   [[0 0] [0 1] [1 0]]    ; 「
   [[0 0] [1 0] [1 1]]    ; └
   [[0 0] [1 0] [1 -1]]]) ; 」

(defn init-board
  ([height width]
   (init-board height width 0))
  ([height width init]
   (vec (repeat height (vec (repeat width init))))))

(defn mapping-triominoes
  [pos]
  (let [map-pos (fn [vector] (map + pos vector))
        map-triomino (fn [triomino] (map map-pos triomino))]
    (map map-triomino triominoes)))

(defn triomino-values
  [board triomino]
  (map #(get-in board %) triomino))

(defn valid-triomino?
  [board triomino]
  (let [valid-pos? (fn [x] (and x (zero? x)))]
    (every? valid-pos? (triomino-values board triomino))))

(defn pos-triominoes
  [board pos]
  (->> (mapping-triominoes pos)
       (filter (partial valid-triomino? board))))

(defn set-triomino
  [board triomino]
  (reduce #(assoc-in %1 %2 1) board triomino))

(defn next-pos
  ([board]
   (let [height (count board)
         width (count (first board))]
     (first (for [i (range height)
                  j (range width)
                  :when (zero? (get-in board [i j]))]
              [i j]))))
  ([board row]
   (let [height (count board)
         width (count (first board))]
     (first (for [j (range width)
                  :when (zero? (get-in board [row j]))]
              [row j])))))

(defn brute-force
  [board]
  (loop [pos (next-pos board)]
    (if (nil? pos)
      1
      (let [triominoes (pos-triominoes board pos)]
        (if (empty? triominoes)
          0
          (apply + (for [triomino triominoes]
                     (brute-force (set-triomino board triomino)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def tiles
  [[2 0 0 0] [0 2 0 0] [0 0 2 0] [0 0 0 2]
   [1 1 0 0] [0 1 1 0] [0 0 1 1] [1 0 0 1] [1 0 1 0] [0 1 0 1]])

(defn match-tiles
  [tiles, nstates, x, y, count, is-bottom?,is-right?]
  (let [above (mod x 3)
        left (mod (quot x (math/expt 3 y)) 3)]
    (apply merge-with +
           nstates
           (for [t tiles
                 :when (and (zero? (mod (+ (get t 0) above) 3))
                            (zero? (mod (+ (get t 3) left) 3))
                            (or (not is-bottom?) (zero? (get t 2)))
                            (or (not is-right?) (zero? (get t 1))))
                 :let [n (+ x (- above) (get t 2) (- (* left (math/expt 3 y))) (* (get t 1) (math/expt 3 y)))]]
             {n count}))))

(defn solve
  [height width]
  (let [states (atom {0 1})]
    (doseq [i (range width)
            j (range height)
            :let [is-bottom? (= (inc j) height)
                  is-right? (= (inc i) width)
                  current-states (reduce (fn [nstates [k v]]
                                           (match-tiles tiles nstates k (inc j) v is-bottom? is-right?))
                                         {} @states)]]
      (reset! states current-states))
    (@states 0)))

(defn euler-161
  []
  (time (solve 9 12)))