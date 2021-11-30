(ns caesarhu.project-euler.euler-155
  (:require [clojure.math.combinatorics :as combo]
            [clojure.core.reducers :as r]
            [net.cgrand.xforms :as x]))

(defn brute-force
  [limit]
  (let [capss (atom [#{} #{1/1}])
        result (atom #{1})]
    (doseq [n (range 2 (inc limit))
            :let [caps (atom #{})]]
      (doseq [a (range 1 (inc (quot n 2)))
              c1 (get @capss a)
              c2 (get @capss (- n a))
              :let [sum (+ c1 c2)]]
        (swap! caps conj sum)
        (swap! caps conj (/ (* c1 c2) sum)))
      (swap! capss conj @caps)
      (swap! result clojure.set/union @caps))
    (count @result)))

(defn calc-capacitor
  [c1 c2]
  (let [sum (+ c1 c2)]
    #{sum (/ (* c1 c2) sum)}))

(defn combine
  [set-a set-b]
  (reduce (fn [acc coll]
            (clojure.set/union acc (apply calc-capacitor coll))) 
          #{} (combo/cartesian-product set-a set-b)))

(defn combine-reducer
  [set-a set-b]
  (->> (combo/cartesian-product set-a set-b)
       (r/map #(apply calc-capacitor %))
       (r/fold clojure.set/union)))

(defn combine-transduce
  [set-a set-b]
  (transduce (x/for [i %
                     j set-a]
                (calc-capacitor i j))
             clojure.set/union set-b))

(defn next-capacitors
  [coll]
  (let [length (count coll)]
    (reduce clojure.set/union
           (for [i (range 1 (inc (quot length 2)))
                 :let [c1 (coll i)
                       c2 (coll (- length i))]]
             (combine-transduce c1 c2)))))

(defn solve
  [limit]
  (loop [capacitors [#{} #{1}]
         result (apply clojure.set/union capacitors)]
    (if (>= (dec (count capacitors)) limit)
      (count result)
      (let [next (next-capacitors capacitors)]
        (recur (conj capacitors next)
               (clojure.set/union result next))))))
