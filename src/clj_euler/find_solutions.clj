(ns clj-euler.find-solutions
  (:use [clj-euler utils])
  (:use [clojure.tools.namespace :only [find-ns-decls-on-classpath]])
  (:use [clojure.string :only [join]]))

(defn- problem-ns? [ns]
  (->> (str ns)
       (re-find #"clj-euler.problem\d+")))

(defn- problem-namespaces []
  (->> (find-ns-decls-on-classpath)
       (map second)
       (filter problem-ns?)))

(defn- solution-fn? [metapath fn]
  (let [md (metapath (meta fn))
        a  (:arguments md)]
    (every? (complement nil?) [md a])))

(defn- solution-fns-in-ns [metapath ns]
  (let [sns (symbol ns)]
    (try (require sns)
         (->> (ns-publics sns)
              (map second)
              (filter ifn?)
              (filter (partial solution-fn? metapath)))
         (catch Exception e nil))))

(defn- single-namespace-solution [ns fns]
  (let [c (count fns)]
    (cond
      (= 1 c) (first fns)
      (= 0 c) (do
                (println "Warning: no appropriate function found in: " ns)
                nil)
      :else (do
              (println "Warning: more than one function found in: " ns " - namely: " (join ", " fns))
              nil))))

(defn- fn->solution [metapath fn]
  (let [solution (metapath (meta fn))
        args     (:arguments solution)]
    {:fn          #(apply fn args)
     :pure-fn     fn
     :problem-num (->> fn
                       str
                       (re-find #"\d+")
                       s->n)
     :expected    (:expected solution)
     :arguments   args}))

(defn solutions-from-namespaces [metapath]
  (doall (->> (problem-namespaces)
              (map (fn [ns] {:fns (solution-fns-in-ns metapath ns)
                            :ns  ns}))
              (map (fn [{:keys [ns fns]}] (single-namespace-solution ns fns)))
              (flatten)
              (filter (complement nil?))
              (map (partial fn->solution metapath)))))
