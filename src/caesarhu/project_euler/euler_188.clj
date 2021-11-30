(ns caesarhu.project-euler.euler-188
  (:require [caesarhu.shun-tools.primes :as p]
            [caesarhu.shun-tools.math-misc :as misc]
            [clojure.math.numeric-tower :as math]))

(defn hyperexponentiation
  [b e m]
  (loop [i 1
         result 1]
    (if (> i e) result
        (recur (inc i) (misc/power-mod b result m)))))

(defn solve
  []
  (let [b 1777
        e 1855
        p (math/expt 10 8)
        m (p/phi p)]
    (let [power (hyperexponentiation b e m)]
      (misc/power-mod b power p))))

(comment
  (time (solve))
  )