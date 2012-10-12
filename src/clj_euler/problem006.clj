(ns clj-euler.problem006)

(defn
  ^{:example  {:expected 2640     :arguments [10]}
    :solution {:expected 25164150 :arguments [100]}}
  solution [n]
  (let [square        #(* % %)
        sum           (partial apply +)
        numbers       (range 1 (inc n))
        sum-of-square (->> numbers (map square) sum)
        square-of-sum (->> numbers sum square)]
    (- square-of-sum sum-of-square)))
