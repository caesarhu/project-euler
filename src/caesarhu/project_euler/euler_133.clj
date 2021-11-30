(ns caesarhu.project-euler.euler-133
  (:require [caesarhu.shun-tools.primes :as p]))

(defn A
  [n]
  (when (p/coprime? 10 n)
    (loop [x 1, k 1]
      (if (zero? (mod x n))
        k
        (recur (mod (inc (* x 10)) n) (inc k))))))

(def prime-A-seq
  (->> (map #(vector % (A %)) p/primes)
       (remove #(nil? (last %)))))

(defn not-factor?
  [a]
  (if (nil? a)
    true
    (-> (frequencies (p/prime-factors-of a))
        (dissoc 2 5)
        not-empty)))

(defn euler-133
  [limit]
  (->> (p/primes-range 0 limit)
       (filter #(not-factor? (A %)))
       (apply +)))
