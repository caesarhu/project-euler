(ns caesarhu.project-euler.euler-068-logic
  (:refer-clojure :exclude [==])
  (:require [clojure.core.logic :refer :all]
            [clojure.core.logic.fd :as fd]
            [caesarhu.project-euler.utils.logic :as l]))

(defn ring
  [n]
  (let [inners (range n)
        outers (range n (* n 2))
        edges (->> (concat inners [(first inners)])
                   (partition 2 1))
        indexs (map #(conj %1 %2) edges outers)
        get-lines (fn [v]
                    (for [is indexs]
                      (for [i is]
                        (v i))))
        vars (vec (lvars (* n 2)))]
    (->> (run* [q]
               (== q vars)
               (everyg #(fd/in % (fd/interval 1 (* n 2))) vars)
               (fd/distinct vars)
               (fresh [sum]
                      (everyg #(l/sumo % sum) (get-lines vars))))
         (map get-lines)
         (filter #(= #{6 7 8 9 10} (set (map first %))))
         (filter #(= 6 (ffirst %)))
         (map #(apply concat %))
         (map #(apply str %))
         sort
         last)))

(comment
  (time (ring 5))
  )