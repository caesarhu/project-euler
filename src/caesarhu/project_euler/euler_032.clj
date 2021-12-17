(ns caesarhu.project-euler.euler-032
  (:require [caesarhu.shun-tools.math-misc :as misc]))

(defn is-pandigital? [s]
  (= "123456789" (apply str (sort s))))

(defn brute-force []
  (reduce +
          (distinct
            (for [a (range 2 5000)
                  b (range a (/ 9999 a))
                  :when (is-pandigital? (str a b (* a b)))]
              (* a b)))))

(defn solve
  []
  (let [pandigital (atom #{})]
    (doseq [i (range 1000 10000)]
      (let [ds (->> (misc/divisors i) rest butlast vec)
            length (count ds)]
        (doseq [j (range (/ length 2))]
          (when (is-pandigital? (str i (ds j) (ds (- length j 1))))
            (swap! pandigital conj i)))))
    (apply + @pandigital)))

(comment
  (time (solve))
  )