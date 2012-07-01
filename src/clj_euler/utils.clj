(ns clj-euler.utils)

(defn s->n [s]
  (Integer/parseInt s 10))

(defn mod-zero? [n d]
  (zero? (mod n d)))
