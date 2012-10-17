(ns clj-euler.problem012
  (:use [clj-euler.primes]))

(defn- x-has-atleast-n-factors? [x n]
  (->> (factors x) (count) (<= n)))

(defn- triangle-numbers
  ([] (triangle-numbers 1 1))
  ([n sum] (let [n' (inc n)
                 sum' (+ sum n')]
             (cons sum (lazy-seq (triangle-numbers n' sum'))))))

(defn
  ^{:example  {:expected 28       :arguments [5]}
    :solution {:expected 76576500 :arguments [500]}}
  first-triangle-number-with-n-divisors [n]
  (->> (triangle-numbers)
       (filter #(x-has-atleast-n-factors? % n))
       first))
