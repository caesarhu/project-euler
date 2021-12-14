(ns caesarhu.project-euler.euler-009)

(defn square
  [n]
  (* n n))

(defn brute-force
  [length]
  (first (for [a (range 1 length)
               b (range (inc a) length)
               :let [c (- length a b)]
               :when (and (< a b c)
                          (> (+ a b) c)
                          (= (+ (square a) (square b)) (square c)))]
           [(* a b c) [a b c]])))

(comment
  (time (brute-force 1000))
  )