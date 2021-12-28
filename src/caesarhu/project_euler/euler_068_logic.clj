(ns caesarhu.project-euler.euler-068-logic
  (:refer-clojure :exclude [==])
  (:require [clojure.core.logic :refer :all]
            [clojure.core.logic.fd :as fd]
            [caesarhu.project-euler.utils.logic :as l]))

(defn magic-3
  []
  (let [vars (vec (lvars 6))
        line-1 [(vars 1) (vars 2) (vars 3)]
        line-2 [(vars 0) (vars 2) (vars 4)]
        line-3 [(vars 0) (vars 1) (vars 5)]]
    (run* [q]
          (everyg #(fd/in % (fd/interval 1 6)) vars)
          (distincto vars)
          (fresh [sum]
                 ()))))