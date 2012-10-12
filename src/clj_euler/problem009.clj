(ns clj-euler.problem009)

(defn- pythagorean-triplet? [a b c]
  (letfn [(square [x] (* x x))]
    (= (square c) (+ (square a) (square b)))))

(defn
  ^{:solution {:expected 31875000 :arguments [1000]}}
  product-of-first-triplet-with-given-sum [s]
  (let [numbers (range 1 (inc s))]
    (->> (for [a numbers
               b numbers
               c numbers
               :let [s' (+ a b c)]
               :when (= s s')
               :when (pythagorean-triplet? a b c)
               :let [product (* a b c)]]
           product)
         first)))
