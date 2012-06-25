(ns clj-euler.primes)

(defn not-divisible-by? [num denum]
  (not (= (mod num denum) 0)))

(defn primes
  ([pos]
     (primes (vec (range 2 pos)) 0))
  ([clist pos]
     (cond
      (>= pos (count clist)) clist
      :else
      (let [val (nth clist pos)
            starting (inc pos)]
        (recur (into (subvec clist 0 starting)
                     (vec (filter #(not-divisible-by? % val)
                                  (subvec clist starting (count clist))))) starting)))))
