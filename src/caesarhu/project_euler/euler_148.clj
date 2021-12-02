(ns caesarhu.project-euler.euler-148
  (:require [clojure.math.numeric-tower :as math]
            [caesarhu.shun-tools.math-misc :as misc]))

; https://projecteuler.net/thread=148;page=8#last
; vikt's solution

(defn solve
  [limit prime]
  (let [prime-prod (quot (* prime (inc prime)) 2)]
    (loop [digits (misc/digits limit prime)
           sum 0
           current-prod 1]
      (if (empty? digits)
        sum
        (let [digit (first digits)]
          (recur (rest digits)
                 (+ (* sum prime-prod) (quot (* current-prod digit (inc digit)) 2))
                 (* (inc digit) current-prod)))))))

(comment
  (time (solve (math/expt 10 9) 7))
  (time (solve 100 2))
  )