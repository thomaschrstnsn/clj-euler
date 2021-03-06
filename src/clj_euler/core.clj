(ns clj-euler.core
  (:use [clj-euler utils runner])
  (:use [clojure.tools.cli :only [cli]])
  (:use [clansi.core :only [style with-ansi without-ansi]]))

(defn- enrich-options [{:keys [examples solutions] :as options} specified-problems]
  (into options {:metapaths   (for [kw {:example  examples
                                        :solution solutions}
                                    :when (second kw)]
                                (first kw))
                 :problem-set (set specified-problems)}))

(defn- parse-problem-args [args]
  (try (doall (map s->n args))
       (catch Exception e
         (do
           (println (style "Failed to parse arguments for problem numbers" :red))
           (System/exit -1)))))

(def opt-def
  [
   ["-a" "--all" "Run functions in all namespaces found." :flag true :default false]
   ["-d" "--dryrun" "Do not actually run anything, just list what would be done." :flag true :default false]
   ["-e" "--examples" "Run the examples." :flag true :default false]
   ["-s" "--solutions" "Run the full problem solutions." :flag true :default true]
   ["-t" "--timing" "Do timing." :flag true :default true]
   ["-v" "--verify" "Verify results from :expected metadata." :flag true :default true]
   ["-c" "--colorize" "Colorize output with ansi-escapes" :flag true :default true]])

(defn- check-options [options banner specified-problems]
  (if (or (options :all)
          ((complement empty?) specified-problems))
    (run (enrich-options options specified-problems))
    (do
      (println (style "clj-euler [switches] ([problem number]*)"
                      :yellow :underline))
      (println "")
      (println (style banner :green)))))

(defn -main [& args]
  (let [[options extra banner] (apply (partial cli args) opt-def)
        specified-problems     (parse-problem-args extra)]
    (if (:colorize options)
      (with-ansi    (check-options options banner specified-problems))
      (without-ansi (check-options options banner specified-problems)))))
