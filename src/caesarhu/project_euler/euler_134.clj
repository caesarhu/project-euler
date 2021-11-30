(ns caesarhu.project-euler.euler-134
  (:require [caesarhu.shun-tools.primes :as p]
            [caesarhu.shun-tools.math-misc :as misc]
            [clojure.math.numeric-tower :as math]))

(defn pair-number
  [[p1 p2]]
  (let [p1-digits (misc/digits p1)]
    (loop [result '()]
      (if (= (count result)
             (count p1-digits))
        (* p2 (misc/to-number result))
        (let [length (inc (count result))
              target (take-last length p1-digits)
              d (first (for [i (range 10)
                             :let [j (misc/to-number (cons i result))
                                   ds (take-last length (misc/digits (* p2 j)))]
                             :when (= target ds)]
                         i))]
          (recur (cons d result)))))))

(defn prime-range
  [limit [p :as prime-seq]]
  (lazy-seq
    (if (> p limit)
      [p]
      (cons p (prime-range limit (rest prime-seq))))))

(defn euler-134
  [limit]
  (->> (prime-range limit (drop 2 p/primes))
       (partition 2 1)
       (map pair-number)
       (apply +)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn extended-gcd
  "The extended Euclidean algorithm
  Returns a list containing the GCD and the Bézout coefficients
  corresponding to the inputs. "
  [a b]
  (cond (zero? a) [(math/abs b) 0 1]
        (zero? b) [(math/abs a) 1 0]
        :else (loop [s 0
                     s0 1
                     t 1
                     t0 0
                     r (math/abs b)
                     r0 (math/abs a)]
                (if (zero? r)
                  [r0 s0 t0]
                  (let [q (quot r0 r)]
                    (recur (- s0 (* q s)) s
                           (- t0 (* q t)) t
                           (- r0 (* q r)) r))))))

(defn chinese_remainder
  " Main routine to return the chinese remainder "
  [n a]
  (let [prod (apply * n)
        reducer (fn [sum [n_i a_i]]
                  (let [p (quot prod n_i)           ; p = prod / n_i
                        egcd (extended-gcd p n_i)   ; Extended gcd
                        inv_p (second egcd)]        ; Second item is the inverse
                    (+ sum (* a_i inv_p p))))
        sum-prod (reduce reducer 0 (map vector n a))] ; Replaces the Python for loop to sum
    ; (map vector n a) is same as
    ;                                             ; Python's version Zip (n, a)
    (mod sum-prod prod)))

(defn crt
  [[p1 p2]]
  (let [k (count (misc/digits p1))]
    (chinese_remainder [(math/expt 10 k) p2] [p1 0])))

(defn euler-134-crt
  [limit]
  (->> (prime-range limit (drop 2 p/primes))
       (partition 2 1)
       (map crt)
       (apply +)))