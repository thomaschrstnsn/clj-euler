(ns clj-euler.primes
  (:require [clojure.contrib.math :as cmath]))

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

(defn primes-upto-including [ceil]
  (take-while #(<= % ceil) (lazy-primes)))

(defn prime?
  "Shamelessly stolen from: http://technicalitee.blogspot.dk/2012/03/project-euler-problem-26-and-27.html
   def isPrime(num: Int) = {
    @tailrec def isPrime0(n: Int): Boolean = n match {
      case 1 => true
      case _ => if (num % n == 0) false else isPrime0(n - 1)
    }
    if (num % 2 == 0 || num < 1)
      false
    else
      isPrime0(Math.sqrt(num).intValue)
  }"
  [n]
  (cond
    (< n 1) false
    (= n 2) true
    :else (loop [f (int (cmath/sqrt n))]
            (if (= f 1)
              true
              (if (= 0 (mod n f))
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

(comment time
 (->> (lazy-primes)
      (take 100000)
      (filter (partial ends-with \0))
      (last)
      ))
