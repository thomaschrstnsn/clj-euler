(ns clj-euler.primes
  (:require [clojure.contrib.math :as cmath])
  (:use [clj-euler.utils]))

(defn lazy-primes
  "shamelessly stolen from http://clj-me.cgrand.net/2009/07/30/everybody-loves-the-sieve-of-eratosthenes/ (lazy-primes3)"
  []
  (letfn [(enqueue [sieve n step]
            (let [m (+ n step)]
              (if (sieve m)
                (recur sieve m step)
                (assoc sieve m step))))
          (next-sieve [sieve candidate]
            (if-let [step (sieve candidate)]
              (-> sieve
                  (dissoc candidate)
                  (enqueue candidate step))
              (enqueue sieve candidate (+ candidate candidate))))
          (next-primes [sieve candidate]
            (if (sieve candidate)
              (recur (next-sieve sieve candidate) (+ candidate 2))
              (cons candidate
                    (lazy-seq (next-primes (next-sieve sieve candidate)
                                           (+ candidate 2))))))]
        (cons 2 (lazy-seq (next-primes {} 3)))))

(defn n-primes [n]
  (take n (lazy-primes)))

(defn primes-below [ceil]
  (take-while #(> ceil %) (lazy-primes)))

(defn prime?
  "Shamelessly inspired from: http://technicalitee.blogspot.dk/2012/03/project-euler-problem-26-and-27.html"
  [n]
  (cond
    (< n 1) false
    (= n 2) true
    :else (loop [f (int (cmath/sqrt n))]
            (if (= f 1)
              true
              (if (mod-zero? n f)
                false
                (recur (dec f)))))))

(defn coprime? [a b]
  (= 1 (cmath/gcd a b)))

(defn- benchmark-primes []
  (time (dotimes [n 10]
          (last (primes-below 100000)))))

(defn- ends-with [c n]
  (let [s  (str n)
        ld (last s)]
    (= c ld)))
