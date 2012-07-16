(ns clj-euler.problem004
  (:use [clj-euler.utils])
  (:require [clojure.contrib.combinatorics :as comb]))

(defn- palindrome? [s]
  (every? true? (map = s (reverse s))))

(defn ^{:example  {:expected 9009   :arguments [10  99]}
        :solution {:expected 906609 :arguments [100 999]}}
  solve [lower upper]
  (let [numbers (range lower (inc upper))]
    (->> (comb/selections (reverse numbers) 2)
         (map (partial apply *))
         (filter (comp palindrome? str))
         (apply max))))
