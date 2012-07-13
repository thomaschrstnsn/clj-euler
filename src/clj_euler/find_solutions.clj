(ns clj-euler.find-solutions
  (:use [clj-euler utils])
  (:use [clojure.tools.namespace :only [find-ns-decls-on-classpath]])
  (:use [clojure.set :only [difference]])
  (:use [clojure.string :only [join]]))

(defn- problem-ns? [ns]
  (->> (str ns)
       (re-find #"clj-euler.problem\d+")))

(defn- problem-namespaces []
  (->> (find-ns-decls-on-classpath)
       (map second)
       (filter problem-ns?)
       set))

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

(defn- fn->solution [metapath ns fn]
  (let [solution (metapath (meta fn))
        args     (:arguments solution)]
    {:fn          #(apply fn args)
     :ns          ns
     :metapath    metapath
     :pure-fn     fn
     :problem-num (->> fn str (re-find #"\d+") s->n)
     :expected    (:expected solution)
     :arguments   args}))

(defn solutions-from-namespaces [metapaths]
  (let [candidate-nss (problem-namespaces)
        solutions     (->> (for [ns candidate-nss
                                 mp metapaths
                                 :let [sol-fns (solution-fns-in-ns mp ns)
                                       sols    (map (partial fn->solution mp ns)
                                                    (filter (complement nil?) sol-fns))]] sols)
                           flatten)
        nss-with-sols (->> solutions (map :ns) set)
        nss-wo-sols   (difference candidate-nss nss-with-sols)]
    (doall (map #(println "Warning: no appropriate function found in: " %) nss-wo-sols))
    (doall solutions)))
