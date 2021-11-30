(ns caesarhu.project-euler.euler-182
  (:require [clojure.math.numeric-tower :as math]
            [caesarhu.shun-tools.math-misc :as misc]
            [caesarhu.shun-tools.primes :as p]
            [injest.classical :refer [x>> =>>]]
            [caesarhu.shun-tools.chinese-remainder :as crt]))

(defn inv-mod
  [e n]
  (when (= (math/gcd e n) 1)
    (-> (crt/extended-gcd e n)
        second
        (mod n))))

(defn ->rsa
  [p q e]
  (let [phi (* (dec p) (dec q))]
    (when-let [d (inv-mod e phi)]
      (when (not= e d)
        {:p p :q q :phi phi :n (* p q) :e e :d d}))))

(defn cipher
  [rsa m]
  (let [{:keys [e n]} rsa]
    (misc/power-mod m e n)))

(defn unconcealed?
  [rsa m]
  (= m (cipher rsa m)))

(defn guess
  [rsa]
  (let [{:keys [phi n]} rsa]
    (->> (range 2 n)
         (filter #(= 1 (math/gcd phi %)))
         (filter #(= 2 (math/gcd phi (dec %))))
         (remove #(= % (inv-mod % phi))))))

(defn calc-unconcealed
  [p q e]
  (* (inc (math/gcd (dec e) (dec p)))
     (inc (math/gcd (dec e) (dec q)))))

(defn solve
  [p q]
  (let [phi (* (dec p) (dec q))]
    (->> (range 2 phi)
         (filter #(= (math/gcd phi %) 1))
         (filter #(= (calc-unconcealed p q %) 9))
         (apply +'))))

(comment
  (time (apply + (guess rsa)))
  (time (solve 1009 3643))
  )