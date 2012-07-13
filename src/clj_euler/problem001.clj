(ns clj-euler.problem001
  (:use [clj-euler.utils]))

(defn- fizzbuzz []
  (let [fizz #(mod-zero? % 3)
        buzz #(mod-zero? % 5)]
    (filter #(or (fizz %)
                 (buzz %))
            (range))))

(defn
  ^{:solution {:expected 233168
               :arguments [1000]}
    :example {:expected 23
              :arguments [10]}}
  fizzbuzz-sum-below-n [n]
  (->> (fizzbuzz)
       (take-while (partial > n))
       (apply +)))
