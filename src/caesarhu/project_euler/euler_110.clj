(ns caesarhu.project-euler.euler-110
  (:require [caesarhu.shun-tools.primes :as p]
            [clojure.math.numeric-tower :as math]
            [clojure.walk :as w]))

(defn exponents->reciprocals
  [exponents]
  (->> (for [[k v] exponents]
         (math/expt (inc (* 2 k)) v))
       (apply *')
       (#(quot (inc %) 2))))

(defn exponents->primes
  [exponents]
  (loop [es (->> (sort exponents)
                 reverse)
         primes p/primes
         result {}]
    (if-let [[k v] (first es)]
      (recur (rest es) (drop v primes) (merge result {k (take v primes)}))
      result)))

(defn exponents->number
  [exponents]
  (->> (exponents->primes exponents)
       (mapcat (fn [[k v]] (map #(math/expt % k) v)))
       (apply *')))

(defn log
  "若p^x大於n，x的最小值"
  [n p]
  (int (math/ceil (/ (Math/log (inc n))
                     (Math/log p)))))

(defn init-exponents
  [n]
  {1 (log (dec (* 2 n)) 3)})

(defn clac-reciprocals-limit
  [limit m]
  (let [r (dec (* limit 2))
        n (->> (map (fn [[k v]] (math/expt (inc (* 2 k)) v)) m)
               (apply *'))]
    (/ r n)))

(defn calc-exponent-limit
  [limit n]
  (->> (iterate #(+ % 2) 3)
       (filter #(> (math/expt % n) limit))
       first
       dec
       (#(quot % 2))))

(defn permutation
  [limit n]
  (let [combination (fn [coll]
                      (let [length (- n (count coll))
                            average (-> (clac-reciprocals-limit limit (frequencies coll))
                                        (calc-exponent-limit length))]
                        (if (= length 1)
                          [(frequencies (cons average coll))]
                          (for [i (range (or (first coll) 1) (inc average))]
                            (cons i coll)))))]
    (loop [[x & xs] (combination [])
           result #{}]
      (cond
        (nil? x) result
        (map? x) (recur xs (conj result x))
        :else (recur (concat (combination x) xs) result)))))

(defn min-exponent
  [limit n]
  (apply min-key exponents->number (permutation limit n)))

(defn solve
  [limit]
  (let [m (reduce (fn [acc n]
                    (let [m (min-exponent limit n)]
                      (if (and acc (< (exponents->number acc) (exponents->number m)))
                        (reduced acc)
                        m)))
                  nil (range (get (init-exponents limit) 1) 0 -1))]
    [(exponents->number m) m]))

(comment
  (time (solve 4000000))
  (time (solve 10000000))
  (time (solve (math/expt 10 100)))
  )
