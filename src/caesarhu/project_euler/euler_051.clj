(ns caesarhu.project-euler.euler-051
  (:require [caesarhu.shun-tools.primes :as p]
            [caesarhu.shun-tools.math-misc :as misc]))

(defn get-repeated-digits [n]
  (->> (misc/digits n)
       frequencies
       (map (fn [[k v]] {v [k]}))
       (apply merge-with concat)))

(defn create-template
  [n replace-digit]
  (map #(when-not (= % replace-digit) %)
       (misc/digits n)))

(defn do-replace-digits
  [template]
  (for [d (range 10)
        :let [replaced (map #(if (nil? %) d %) template)]
        :when (-> replaced first zero? not)]
    (misc/to-number replaced)))

(defn generate-replaced-primes
  [num-matches p]
  (->> (for [[num-digits digit-seq] (get-repeated-digits p)
             replace-digit digit-seq
             :let [template (create-template p replace-digit)]]
         (do-replace-digits template))
       (map #(filter p/is-prime? %))
       (some #(and (= num-matches (count %)) %))))

(defn solve
  [num-matches]
  (->> (p/primes-after 10)
       (some #(generate-replaced-primes num-matches %))
       first
       int))

; (time (solve 8))