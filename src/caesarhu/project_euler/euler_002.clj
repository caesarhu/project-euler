(ns caesarhu.project-euler.euler-002)

(def fibonacci
  ((fn fib [a b]
     (lazy-seq (cons a (fib b (+ a b)))))
   0 1))

(defn brute-force
  [n]
  (->> (take-while #(< % n) fibonacci)
       (filter even?)
       (apply +)))

(defn brute-force2
  [n]
  (->> (take-nth 3 fibonacci)
       (take-while #(< % n))
       (apply +)))

(def even-fibonacci
  ((fn fib [a b]
     (lazy-seq (cons a (fib b (+ a (* 4 b))))))
   2 8))

(defn solve
  [n]
  (apply + (take-while #(< % n) even-fibonacci)))

(comment
  "一、clojure 提供 lazy-seq，首先建立一個 lazy-seq fibonacci
   二、直覺的想法，過濾為 even 的 fibonacci 數列，再加總即可
   三、再進一步，even 的 fibonacci 數列為每隔3個，稍微優化為 (take-nth 3 fibonacci)
   四、最後，even 的 fibonacci 數列可導出公式 f(n) = 4(f(n-1)) + f(n-2)，以此公式建立偶數的fibonacci lazy-seq，再加總
   五、因 fibonacci 數列的性質，雖然 f(n-1)/f(n) 趨近於黃金分割，但仍有誤差，無法以等比數列的方式求解"
  (time (solve 4000000))
  (time (brute-force2 4000000))
  )