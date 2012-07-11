(ns clj-euler.core
  (:use [clj-euler.utils])
  (:use [clojure.set :only [difference]])
  (:use [clojure.string :only [join]])
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

(defn- time-fn [{:keys [timing dryrun fname verify]}
                {:keys [fn problem-num expected]}]
  (let [timer (if timing
                #(time (%))
                #(%))]
    (println (str "-> " fname ": " problem-num))
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

(defn- fn->problem-map [fn]
  {:fn fn
   :problem-num (fn->problem-number fn)
   :expected ((meta fn) :expected)})

(defn- problem-maps-from-namespaces [fn-symbol num-predicate]
  (->> (problem-namespaces)
       (map (partial fn-in-ns fn-symbol))
       (filter (complement nil?))
       (map fn->problem-map)))

(defn- parse-problem-args! [args]
  (try (doall (map s->n args))
       (catch Exception e
         (do
           (println "Failed to parse arguments for problem numbers")
           (System/exit -1)))))

(defn- run [{:keys [fname problem-set all] :as options}]
  (let [num-predicate     (if all
                            (fn [n] true)
                            (fn [n] (contains? problem-set n)))
        all-problem-maps  (problem-maps-from-namespaces fname num-predicate)
        specified-missing (difference problem-set
                                      (set (map :problem-num all-problem-maps)))
        filtered-pms      (filter (comp num-predicate :problem-num) all-problem-maps)
        time-fns          (map (partial time-fn options) filtered-pms)]
    (when-not (empty? specified-missing)
      (println "Specified problems not implemented: " (join ", " specified-missing)))
    (doall time-fns)
    nil))

(defn- enrich-options [{:keys [example] :as options} specified-problems]
  (into options {:fname (if example 'example 'problem)
                 :problem-set (set specified-problems)}))

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
