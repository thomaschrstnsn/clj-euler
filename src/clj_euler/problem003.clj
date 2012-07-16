(ns clj-euler.problem003
  (:use [clj-euler primes utils])
  (:require [clojure.contrib.math :as cmath]))

(defn ^{:solution {:expected 6857
                   :arguments [600851475143]}
        :example {:expected 29
                  :arguments [13195]}}
  greatest-prime-factor-of-n [n]
  (let [limit (int (cmath/sqrt n))]
    (->> (primes-below limit)
         (filter (partial mod-zero? n))
         last)))
