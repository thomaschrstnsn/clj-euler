(ns clj-euler.problem002
  (:use [clj-euler.utils]))

(defn- fib
  ([] (cons 1 (cons 2 (lazy-seq (fib 1 2)))))
  ([x y]
     (let [n (+ x y)]
       (cons n (lazy-seq (fib y n))))))

(defn ^{:example {:arguments [10]
                  :expected [1, 2, 3, 5, 8, 13, 21, 34, 55, 89]}}
  first-n-fibs [n]
  (apply vector (take n (fib))))

(defn ^{:solution {:arguments [(int 4e6)]
                   :expected 4613732}}
  sum-even-fibs-below-n [n]
  (->> (fib)
       (take-while (partial > n))
       (filter even?)
       (apply +)))
