(ns clj-euler.core
  (:use [clojure.tools.namespace :only [find-ns-decls-on-classpath]]))

(defn- problem-ns? [ns]
  (->> (str ns)
       (re-find #"clj-euler.problem.*")))

(defn problem-namespaces []
  (->> (find-ns-decls-on-classpath)
       (map second)
       (filter problem-ns?)))

(defn- fn-in-ns [fn ns]
  (let [sns (symbol ns)]
    (try (require sns)
         (let [r (ns-resolve sns fn)]
           (if (ifn? r)
             r
             nil))
         (catch Exception e nil))))

(defn- ex-wrap [fn]
  (try (fn)
       (catch Exception e (str "Caught exception " (.getMessage e)))))

(defn- time-fn [fn]
  (println (str "-> " fn))
  (println (str "==> " (time (ex-wrap fn)))))

(defn- fns-in-problem-namespaces [fn-symbol]
  (->> (problem-namespaces)
       (map (partial fn-in-ns fn-symbol))
       (filter (complement nil?))
       (map time-fn)
       (doall))
  nil)

(defn -main
  ([] (fns-in-problem-namespaces 'problem))
  ([an-arg & other-args] (println "invalid number of arguments...")))
