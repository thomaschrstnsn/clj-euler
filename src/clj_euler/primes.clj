(ns clj-euler.primes)

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
