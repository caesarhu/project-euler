(ns caesarhu.project-euler.euler-121)

(defn solve
  [turns prob red blue]
  (if (zero? turns)
    (if (> blue red) prob 0)
    (let [prob-red (* prob (/ turns (inc turns)))
          prob-blue (* prob (/ 1 (inc turns)))]
      (+ (solve (dec turns) prob-red (inc red) blue)
         (solve (dec turns) prob-blue red (inc blue))))))

(defn euler-121
  [turns]
  (->> (solve turns 1 0 0)
       (/ 1)
       (int)))
