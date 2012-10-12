(ns clj-euler.problem005
  (:use [clj-euler.utils]))

(defn
  ^{:example  {:expected 2520      :arguments [1 10]}
    ;; :solution {:expected 232792560 :arguments [1 100]}
    }
  naïve-solution [start end]
  (let [numbers  (range start (inc end))
        modzs    #(map (partial mod-zero? %) numbers)
        allmodzs #(every? identity (modzs %))]
    (->> (range)
         rest
         (filter allmodzs)
         first)))
