(ns clj-euler.problem014)

(defn- next-in-sequence [n]
  (cond
   (= n 1) nil
   (even? n) (/ n 2)
   :else (inc (* 3 n))))

(defn- the-sequence [n]
  (let [n' (next-in-sequence n)]
    (if (nil? n')
      [n]
      (cons n (lazy-seq (the-sequence n'))))))

(defonce ^:dynamic *cache* (atom {}))

(defn- cached-sequence-length [n]
  (let [cached (get @*cache* n)]
    (if (nil? cached)
      (let [n' (next-in-sequence n)]
        (if (nil? n')
          1
          (let [l' (cached-sequence-length n')
                l  (inc l')]
            (reset! *cache* (assoc @*cache* n l))
            l)))
      cached)))

(defn
  ^{:solution {:expected 837799 :arguments [1000000]}}
  cached-solution [limit]
  (do
    (dorun (->> (range 1 limit) (map cached-sequence-length)))
    (->> @*cache* (sort-by second) (last) (first))))

(defn- sequence-length
  [^Long n ^Long l]
  (let [n' (next-in-sequence n)]
    (if (nil? n')
      l
      (recur n' (inc l)))))

(defn
  ^{:solution {:expected 837799 :arguments [1000000]}}
  memoized-solution [limit]
  (let [seq-length (memoize sequence-length)]
    (->> (range 1 limit)
         (map #(vector (seq-length % 0) %))
         (flatten)
         (apply sorted-map)
         (last)
         (second))))

(defn- ^long imp-helper [^long j ^long this_terms]
  (if (not= j 1)
    (cond
     (= 0 (rem j 2)) (recur (bit-shift-right j 1) (unchecked-inc this_terms))
     :else (recur (unchecked-inc (unchecked-multiply j 3)) (unchecked-inc this_terms)))
    this_terms))

(defn
  ^{:solution {:expected 837799 :arguments [1000000]}}
  ^long imperative [^long limit]
  (loop [longest 0
         terms 0
         i 1]
    (if (<= i limit)
      (let [this_terms (imp-helper i 1)]
        (if (< terms this_terms)
          (recur i this_terms (unchecked-inc i))
          (recur longest terms (unchecked-inc i))))
      longest)))
