(ns clj-euler.core
  (:use [clj-euler.utils])
  (:use [clojure.set :only [difference]])
  (:use [clojure.string :only [join]])
  (:use [clojure.tools.namespace :only [find-ns-decls-on-classpath]])
  (:use [clojure.tools.cli :only [cli]]))

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

(defn- ex-wrap [fn]
  (try (fn)
       (catch Exception e (str "Caught exception " (.getMessage e)))))

(defn- time-fn [{:keys [timing dryrun metapath verify]}
                {:keys [fn problem-num expected arguments]}]
  (let [timer (if timing
                #(time (%))
                #(%))]
    (println (str "-> " (name metapath) ": " problem-num))
    (when-not dryrun
      (let [result      (timer (partial ex-wrap fn))
            noexpected? (and verify
                             (nil? expected))
            unexpected? (and verify
                             ((complement nil?) expected)
                             (not= result expected))]
        (println (str "==> " result
                      (when unexpected?
                        (str " IS NOT EQUALING " expected ", which was expected"))
                      (when noexpected?
                        (str " (no expected result)"))))))))

(defn- fn->problem-number [fn]
  (->> fn
       str
       (re-find #"\d+")
       s->n))

(defn- fn->solution [metapath fn]
  (let [solution (metapath (meta fn))
        args     (:arguments solution)]
    {:fn          #(apply fn args)
     :pure-fn     fn
     :problem-num (fn->problem-number fn)
     :expected    (:expected solution)
     :arguments   args}))

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

(defn- solutions-from-namespaces [metapath]
  (doall (->> (problem-namespaces)
              (map (fn [ns] {:fns (solution-fns-in-ns metapath ns)
                            :ns  ns}))
              (map (fn [{:keys [ns fns]}] (single-namespace-solution ns fns)))
              (flatten)
              (filter (complement nil?))
              (map (partial fn->solution metapath)))))

(defn- run [{:keys [metapath problem-set all] :as options}]
  (let [all-solutions     (solutions-from-namespaces metapath)
        specified-missing (difference problem-set
                                      (set (map :problem-num all-solutions)))

        num-predicate     (if all
                            (fn [n] true)
                            (fn [n] (contains? problem-set n)))

        filtered-pms      (filter (comp num-predicate :problem-num) all-solutions)
        time-fns          (map (partial time-fn options) filtered-pms)]
    (when-not (empty? specified-missing)
      (println "Specified problems not implemented: " (join ", " specified-missing)))
    (doall time-fns)
    nil))

(defn- enrich-options [{:keys [example] :as options} specified-problems]
  (into options {:metapath (if example :example :solution)
                 :problem-set (set specified-problems)}))

(defn- parse-problem-args! [args]
  (try (doall (map s->n args))
       (catch Exception e
         (do
           (println "Failed to parse arguments for problem numbers")
           (System/exit -1)))))

(defn -main [& args]
  (let [[options extra banner] (cli args
                                    ["-a" "--all" "Run all." :flag true :default false]
                                    ["-d" "--dryrun" "Do not actually run anything, just list what would be done." :flag true :default false]
                                    ["-e" "--example" "Only run the examples (not the full problem)." :flag true :default false]
                                    ["-t" "--timing" "Do timing." :flag true :default true]
                                    ["-v" "--verify" "Verify results from :expected metadata." :flag true :default true])
        specified-problems     (parse-problem-args! extra)]
    (when-not (or (options :all) ((complement empty?) specified-problems))
      (println "clj-euler [switches] ([problem number]*)")
      (println "")
      (println banner)
      (System/exit 0))
    (run (enrich-options options specified-problems))))
