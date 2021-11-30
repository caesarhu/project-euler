(ns caesarhu.project-euler.euler-063
  (:require [clojure.math.numeric-tower :as math]
            [caesarhu.shun-tools.math-misc :as misc]))

(defn power-digits
  [n power]
  (->> (math/expt n power)
       misc/digits
       count))

(defn x-power-digits
  [power]
  (let [is-power=digits? (fn [n]
                           (= power (power-digits n power)))]
    (->> (take 100 misc/integers)
         (drop-while #(not (is-power=digits? %)))
         (take-while is-power=digits?))))

(defn solve
  []
  (->> misc/integers
       (map x-power-digits)
       (take-while not-empty)
       flatten
       count))

; (time (solve))