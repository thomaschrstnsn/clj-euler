(ns clj-euler.problem035
  (:use clj-euler.primes
        clj-euler.utils
        clojure.contrib.seq :as cseq))

(defn- rotations-of-number [n]
  (->> (str n)
       (cseq/rotations)
       (map (comp s->i (partial apply str)))
       ))

(defn- contains-all-rotations? [s n]
  (let [rs (rotations-of-number n)]
    (every? (partial contains? s) rs)))

(defn circular-primes-below [n]
  (let [primes (->> (primes-below n)
                    (into #{}))]
    (filter (partial contains-all-rotations? primes) primes)))

(defn example []
  (circular-primes-below 100))

(defn problem []
  (->> 1000000 circular-primes-below count))
