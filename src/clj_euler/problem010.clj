(ns clj-euler.problem010
  (:use [clj-euler.primes]))

(defn ^{:example  {:expected 17           :arguments [10]}
        :solution {:expected 142913828922 :arguments [2000000]}}
  sum-of-primes-below [n]
  (->> (primes-below n)
       (apply +)))
