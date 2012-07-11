(ns clj-euler.problem027
  (:require [clojure.contrib.combinatorics :as comb])
  (:require [clojure.contrib.math :as cmath])
  (:use clj-euler.primes)
  (:use [clojure.contrib.greatest-least :only [greatest-by]]))

(defn quadratic [a b n]
  (+ (* n n) (* a n) b))

(defn quadratic-from-pair [[a b]]
  (let [q (partial quadratic a b)]
    {:a a :b b :q q}))

(def memo-prime? (memoize prime?))

(defn consecutive-prime-results [q]
  (last (take-while #(memo-prime? (q %)) (range))))

(defn inclusive-range [start end]
  (range start (inc end)))

(defn find-quad-with-most-consec-primes [as bs]
  (let [candidates (for [a as, b bs]
                     {:a a, :b b, :qf (partial quadratic a b)})]
    (loop [[q & qs] candidates
           best {:cp -1}]
      (if (nil? q)
        best
        (let [{:keys [a b]} q
              bcp (:cp best)]
          (if (< (cmath/abs b) bcp)
            (recur qs best)
            (let [qcp   (consecutive-prime-results (:qf q))
                  q'    (assoc q :cp qcp)
                  best' (if (or (nil? qcp) (>= bcp qcp))
                          best
                          q')]
              (recur qs best'))))))))

(defn solve-with-bounds [b]
  (let [min (- b)
        max b
        r (inclusive-range min max)]
    (find-quad-with-most-consec-primes r (primes-below b))))

(def q39 (partial quadratic 1 41))
(def q40 (partial quadratic -1 41))
(def q79 (partial quadratic -79 1601))

(defn ^{:expected -59231}
  problem []
  (let [{:keys [a b]} (solve-with-bounds 1000)]
    (* a b)))
