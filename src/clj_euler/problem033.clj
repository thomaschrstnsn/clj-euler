(ns clj-euler.problem033
  (:use [clj-euler.utils])
  (:use [clojure.string :only [join]]))

(defn can-phony-cancel? [num den]
  (let [sn (set (str num))
        sd (set (str den))
        overlap (some (into sn "0") sd)]
    (if (or (nil? overlap)
            (= overlap \0)
            (= 1 (count sn)) (= 1 (count sd)))
      nil
      overlap)))

(defn phony-cancel [n cancel-digit]
  (->> (str n)
       (filter (partial not= cancel-digit))
       (join "")
       (s->n)))

(defn
  ^{:expected 100}
  problem []
  (let [r (range 10 100)
        candidates (for [n r, d r
                         :let [pc (can-phony-cancel? n d)]
                         :when (and (< n d) pc)]
                     {:n n :d d :pc pc :cn (phony-cancel n pc) :cd (phony-cancel d pc)})]
    (->> candidates
         (filter (fn [{:keys [n d cn cd]}] (= (/ n d) (/ cn cd))))
         (map (fn [{:keys [n d]}] (/ n d)))
         (apply *)
         (denominator))))
