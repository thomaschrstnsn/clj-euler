(ns clj-euler.problem016
  (:use [clj-euler.utils :only [c->n]]))

(defn ^{:example  {:expected 26   :arguments [2 15]}
        :solution {:expected 1366 :arguments [2 1000]}}
  sum-of-digits-in-n-pow [n power]
  (->> (-> (biginteger n) (.pow power) str)
       (map c->n)
       (reduce +)))
