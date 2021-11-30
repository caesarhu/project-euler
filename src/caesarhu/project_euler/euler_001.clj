(ns caesarhu.project-euler.euler-001)

(defn brute-force
  [n]
  (->> (filter (fn [x] ((some-fn #(zero? (mod % 3)) #(zero? (mod % 5))) x)) (range 1 n))
       (apply +)))

(defn sum
  [limit step]
  (let [length (quot limit step)
        max-step (* step length)]
    (quot (* (+ step max-step) length) 2)))

(defn solve
  [limit]
  (+ (sum (dec limit) 3) (sum (dec limit) 5) (- (sum (dec limit) 15))))

(comment
  (time (brute-force 100000000))
  (time (solve 100000000))
  "本題當然 brute-force 速度也很快，但 brute-force 的時間複雜度是 O(n)，而 solve 的時間複雜度是 O(3)，
   例如當數字大到 100000000 時，差異就很明顯。"
  )