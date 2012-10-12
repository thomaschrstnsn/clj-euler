(ns clj-euler.utils)

(defn s->n [^String s]
  (Integer/parseInt s 10))

(defn c->n [^Character c]
  (Integer/parseInt (str c) 10))

(defn mod-zero? [n d]
  (zero? (mod n d)))
