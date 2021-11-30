(ns caesarhu.project-euler.euler-032)

(defn is-pandigital? [s]
  (= "123456789" (apply str (sort s))))

(defn solve []
  (reduce +
          (distinct
            (for [a (range 2 5000)
                  b (range a (/ 9999 a))
                  :when (is-pandigital? (str a b (* a b)))]
              (* a b)))))

(comment
  (time (solve))
  )