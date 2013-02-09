(ns clj-euler.problem015)

(defn- transistion [{:keys [entry column rows]}]
  (let [e+ (/ (* entry (- (inc (* 2 rows)) column)) column)
        c+ (inc column)]
    {:entry e+
     :column c+
     :rows rows}))

(defn
  ^{:solution {:expected 137846528820
               :arguments [20 20]}
    :example {:expected 6
              :arguments [2 2]}}
  lattice-paths [rows columns]
  (->> {:entry 1
        :column 1
        :rows rows}
       (iterate transistion)
       (drop-while #(<= (:column %) columns))
       first
       :entry))
