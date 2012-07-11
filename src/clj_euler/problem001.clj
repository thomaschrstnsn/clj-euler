(ns clj-euler.problem001
  (:use [clj-euler.utils]))

(defn- fizzbuzz []
  (let [fizz #(mod-zero? % 3)
        buzz #(mod-zero? % 5)]
    (filter #(or (fizz %)
                 (buzz %))
            (range))))

(defn- fizzbuzz-sum-below-n [n]
  (->> (fizzbuzz)
       (take-while (partial > n))
       (apply +)))

(defn ^{:expected 233168}
  problem []
  (fizzbuzz-sum-below-n 1000))
