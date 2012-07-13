(ns clj-euler.core
  (:use [clj-euler utils runner])
  (:use [clojure.tools.cli :only [cli]]))

(defn- enrich-options [{:keys [example] :as options} specified-problems]
  (into options {:metapath (if example :example :solution)
                 :problem-set (set specified-problems)}))

(defn- parse-problem-args [args]
  (try (doall (map s->n args))
       (catch Exception e
         (do
           (println "Failed to parse arguments for problem numbers")
           (System/exit -1)))))

(def opt-def
  [
   ["-a" "--all" "Run functions in all namespaces found." :flag true :default false]
   ["-d" "--dryrun" "Do not actually run anything, just list what would be done." :flag true :default false]
   ["-e" "--example" "Only run the examples (not the full problem)." :flag true :default false]
   ["-t" "--timing" "Do timing." :flag true :default true]
   ["-v" "--verify" "Verify results from :expected metadata." :flag true :default true]
   ])

(defn -main [& args]
  (let [[options extra banner] (apply (partial  cli args) opt-def)
        specified-problems     (parse-problem-args extra)]
    (when-not (or (options :all) ((complement empty?) specified-problems))
      (println "clj-euler [switches] ([problem number]*)")
      (println "")
      (println banner)
      (System/exit 0))
    (run (enrich-options options specified-problems))))
