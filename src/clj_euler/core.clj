(ns clj-euler.core
  (:use [clj-euler.utils])
  (:use [clojure.tools.namespace :only [find-ns-decls-on-classpath]])
  (:use [clojure.tools.cli :only [cli]]))

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

(defn- time-fn [ {:keys [timing]} {:keys [fn problem]}]
  (let [timer (if timing
                #(time (%))
                #(%))]
    (println (str "-> problem: " problem))
    (println (str "==> " (timer (partial ex-wrap fn))))))

(defn- fn->problem-number [fn]
  (->> fn
       str
       (re-find #"\d+")
       s->n))

(defn- fn->problem-map [fn]
  {:fn fn
   :problem (fn->problem-number fn)})

(defn- fns-in-problem-namespaces [fn-symbol num-predicate options]
  (->> (problem-namespaces)
       (map (partial fn-in-ns fn-symbol))
       (filter (complement nil?))
       (map fn->problem-map)
       (filter (comp num-predicate :problem))
       (map (partial time-fn options))
       (doall))
  nil)

(defn- parse-problem-args! [args]
  (try (doall (map s->n args))
       (catch Exception e
         (do
           (println "Failed to parse arguments for problem numbers")
           (System/exit -1)))))

(defn- run [options]
  (let [fname (if (options :example) 'example 'problem)
        problem-set (set (options :problems))
        num-predicate (if (options :all)
                        (fn [n] true)
                        (fn [n] (contains? problem-set n)))]
    (fns-in-problem-namespaces fname num-predicate options)))

(defn -main [& args]
  (let [[options extra banner] (cli args
                                    ["-e" "--example" "Only run the examples (not the full problem)." :flag true :default false]
                                    ["-a" "--all" "Run all." :flag true :default false]
                                    ["-t" "--timing" "Do timing." :flag true :default true])
        specified-problems (parse-problem-args! extra)]
    (when-not (or (options :all) ((complement empty?) specified-problems))
      (println "[switches] ([problem number]*)")
      (println "")
      (println banner)
      (System/exit 0))
    (run (assoc options :problems specified-problems))))
