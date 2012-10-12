(ns clj-euler.problem007
  (:use [clj-euler.primes]))

(defn
  ^{:example  {:expected 13     :arguments [6]}
    :solution {:expected 104743 :arguments [10001]}}
  nth-prime [n]
  (->> (lazy-primes)
       (drop (dec n))
       first))
