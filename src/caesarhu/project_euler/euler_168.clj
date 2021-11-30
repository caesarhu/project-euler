(ns caesarhu.project-euler.euler-168
  (:require [clojure.math.numeric-tower :refer [gcd expt]]))

; https://projecteuler.net/thread=168;page=7#last
; tabidots's solution

(defn ord-10
  "Naive algorithm to find ord_10 mod p (the multiplicative order of 10 mod p),
  when p is prime. This is the smallest positive integer k such that 10^k ≡ 1 mod p."
  [p]
  (when (= 1 (gcd 10 p))
    (->> (iterate #(mod (* 10 %) p) 10)
         (take-while #(> % 1))
         (count)
         (inc)))) ;; we want the next iteration where a^k DOES equal 1

(defn parasitic
  "Given n ≥ k where both n, k are single digits, generates the unique primitive
  n-parasitic number whose ones digit is k. Returns it as a string, as some of them
  are very long."
  ;; This algo was adapted from the one to generate cyclic numbers
  [k n]
  (when (<= 1 n k 9)
    ;; reduce k/(10n-1) to lowest terms
    ;; e.g., when k=7,n=5, you end up with 7/49, which will produce extra repetitions
    ;; and not a primitive parasitic number if the algo runs as-is.
    (let [k'            (numerator   (/ k (dec (* 10 n))))
          denom         (denominator (/ k (dec (* 10 n))))
          period-length (ord-10 denom)]
      (->> [denom k']
           (iterate (fn [[q r]]
                      [(quot (* 10 r) denom) (rem (* 10 r) denom)]))
           (drop 1)
           (take period-length)
           (map first)
           (apply str)))))

(def primitive-parasitics
  "Primitive parasitic numbers that are NOT repdigits."
  (for [k (range 1 10)
        n (range 2 10)
        :let [pnum (parasitic k n)]
        :when (some? pnum)]
    pnum))

(defn last-5-digits
  "Takes the last 5 digits of a number string and returns it as an integer."
  [num-string]
  (Integer/parseInt (apply str (take-last 5 num-string))))

(defn make-repdigits
  "Make all repdigits of a single digit `d` that have at least `min-digits`
  and at most `max-digits`. For example, (9 2 4) returns (99 999 9999)."
  [d min-digits max-digits]
  (when (<= 1 d 9)
    (->> (str d)
         (iterate (partial str d))
         (take max-digits)
         (drop (dec min-digits))
         (map read-string))))

(defn sum-rotatables
  "Returns the last `sig-digits` of the sum of ALL numbers that are divisors
  of their right-rotation from 10 to 10^n. This can include concatenations of primitive
  parasitic numbers, such as 142857142857 and 142857142857142857..."
  [n sig-digits]
  (let [sum-pps                (->> primitive-parasitics
                                    (map (fn [p] (* (last-5-digits p)
                                                    (quot n (count p)))))
                                    (reduce +'))
        sum-unique-repdigits   (->> (range 1 10)
                                    (mapcat #(make-repdigits % 2 (dec sig-digits)))
                                    (reduce +'))
        num-reps               (inc (- n sig-digits)) ;; 96
        sum-repeated-repdigits (->> (range 1 10)
                                    (map #(* (read-string (apply str (repeat sig-digits %)))
                                             num-reps))
                                    (reduce +'))]
    (mod (+ sum-pps
            sum-unique-repdigits
            sum-repeated-repdigits)
         (expt 10 sig-digits))))

(defn problem-168
  "Find the last 5 digits of the sum of all integers n, 10 < n < 10^100,
  that are divisors of their right-rotations."
  []
  (sum-rotatables 100 5))

(comment
  (time (problem-168))
  )