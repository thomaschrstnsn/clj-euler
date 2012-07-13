(ns clj-euler.problem026
  (:use clj-euler.primes
        clojure.contrib.greatest-least))

(defn construct-cyclic-number
  "See http://en.wikipedia.org/wiki/Cyclic_number#Construction_of_cyclic_numbers"
  ([p]   (construct-cyclic-number 10 p))
  ([b p]
     (loop [t 1, r 1, n 0]
       (let [x  (*' r b)
             d  (unchecked-divide-int x p)
             r' (mod x p)
             n' (+' (*' n b) d)]
         (if (= r' 1)
           n'
           (recur (inc t) r' n'))))))

(defn digits-in-cyclic-number-of-n [n]
  (let [cn (construct-cyclic-number n)]
    (count (str cn))))

(defn
  ^{:solution {:expected 983 :arguments [1000]}
    :example  {:expected 7   :arguments [10]}}
  unit-fraction-below-n-with-most-digits [n]
  (apply
   greatest-by digits-in-cyclic-number-of-n
   (drop-while (partial > 7) (primes-below n))))
