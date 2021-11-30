(ns caesarhu.project-euler.euler-176
  (:require [caesarhu.shun-tools.primes :as p]
            [clojure.math.numeric-tower :as math]))

(defn solve
  []
  (let [target 47547
        sum (inc (* 2 target))
        [x & more :as prime-expt] (->> (p/prime-factors-of sum)
                                       (map dec))]
    (->> (map #(math/expt %1 %2) p/primes prime-expt)
         (apply *')
         (#(if (>= x 2) (*' 4 %) %))
         math/sqrt)))

(comment
  (time (solve))
  )