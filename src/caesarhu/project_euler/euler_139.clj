(ns caesarhu.project-euler.euler-139
  (:require [clojure.math.numeric-tower :as math]
            [caesarhu.shun-tools.primes :as p]
            [caesarhu.shun-tools.pell-equation :as pell]))

(defn generate-triangle
  [limit]
  (for [m (range 1 (math/sqrt limit) 2)
        n (range 1 m 2)
        :when (p/coprime? m n)
        :let [m2 (* m m)
              n2 (* n n)]
        :when (zero? (mod (+ m2 n2) (- m2 n2 (* 2 m n))))]
    (* m (+ m n))))

(defn euler-139
  [limit]
  (->> (generate-triangle limit)
       (map #(quot limit %))
       (apply +)))

; (time (euler-139 100000000))

(defn euler-139-pell
  [limit]
  (->> (rest (pell/pell-solutions 2 -1))
       (map (partial apply +))
       (take-while #(< % limit))
       (map #(quot limit %))
       (apply +)))

; (time (euler-139-pell 100000000))