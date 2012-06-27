(ns clj-euler.core
  (:use clj-euler.primes))

(defn- m
  ([] (m 1000))
  ([n] (println
        (str "Last primes before "
             n
             ": "
             (last (primes-below n))))))

(defn- s->i [s] (Integer/parseInt s 10))

(defn -main
  ([] (m))
  ([arg] (let [ia (s->i arg)] (m ia)))
  ([an-arg & other-args] (println "invalid number of arguments...")))
