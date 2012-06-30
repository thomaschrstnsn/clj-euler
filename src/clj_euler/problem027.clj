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

(def prime?-memo (memoize prime?))

(defn consecutive-prime-results [q]
  (last (take-while #(prime?-memo (q %)) (range))))

(defn inclusive-range [start end]
  (range start (inc end)))

(comment defn find-quad-with-most-consec-primes [as bs]
  (->>                       ;(comb/cartesian-product as bs)
                                        ;(filter #(let [[a b] %] (prime? (* a b))))
                                        ;(filter #(let [[a b] %] (coprime? a b)))
                                        ;(map quadratic-from-pair)
   (for [a as, b bs
                                        ;:when
                                        ;         (and (prime? (* a b))             (coprime? a b))
         ]
     {:a a :b b :q (partial quadratic a b)})
   (apply greatest-by #(consecutive-prime-results (% :q)))
   ))

(defn find-quad-with-most-consec-primes [as bs]
  (let [candidates (for [a as, b bs
                         :let [aa (cmath/abs a)
                               ab (cmath/abs b)]
                         ;:when (coprime? aa ab)
                         ]
                     {:a a, :b b, :qf (partial quadratic a b)})]
    (loop [[q & qs] candidates
           best {:cp -1}
           round 0]
      (if (nil? q)
        best
        (let [{:keys [a b]} q
              bcp (:cp best)
              round' (inc round)]
          (if (< (cmath/abs b) bcp)
            (recur qs best round')
            (let [qcp   (consecutive-prime-results (:qf q))
                  q'    (assoc q :cp qcp)
                  best' (if (or (nil? qcp) (>= bcp qcp))
                          best
                          q')]
              (do
                (comment when (= 0 (mod round 1000))
                  (println (str "r: " round " " q)))
                (recur qs best' round'))))
          )))))

(defn solve-with-bounds [b]
  (let [min (- b)
        max b
        r (inclusive-range min max)]
    (find-quad-with-most-consec-primes r (primes-below b))))

(def q39 (partial quadratic 1 41))
(def q40 (partial quadratic -1 41))
(def q79 (partial quadratic -79 1601))

(defn problem []
  (let [{:keys [a b]} (time (solve-with-bounds 1000))]
    (* a b)))

(defn -main []
  (println (problem)))
