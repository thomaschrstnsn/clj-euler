(ns clj-euler.runner
  (:use [clj-euler utils find-solutions])
  (:use [clojure.set :only [difference]])
  (:use [clojure.string :only [join]]))

(defn- exception-wrap [fn]
  (try (fn)
       (catch Exception e (str "Caught exception " (.getMessage e)))))

(defn- run-solution-fn [{:keys [timing dryrun metapath verify]}
                {:keys [fn problem-num expected arguments pure-fn]}]
  (let [timer (if timing
                #(time (%))
                #(%))
        fname (:name (meta pure-fn))]
    (println (str "\n-> [problem " problem-num "/" (name metapath) "] " fname))
    (when-not dryrun
      (let [result      (timer (partial exception-wrap fn))
            noexpected? (and verify
                             (nil? expected))
            unexpected? (and verify
                             ((complement nil?) expected)
                             (not= result expected))]
        (println (str "<- " result
                      (when unexpected?
                        (str " IS NOT EQUALING " expected ", which was expected"))
                      (when noexpected?
                        (str " (no expected result)"))))))))

(defn run [{:keys [metapath problem-set all] :as options}]
  (let [all-solutions     (solutions-from-namespaces metapath)
        specified-missing (difference problem-set
                                      (set (map :problem-num all-solutions)))

        num-predicate     (if all
                            (fn [n] true)
                            (fn [n] (contains? problem-set n)))

        filtered-sols     (filter (comp num-predicate :problem-num) all-solutions)
        time-fns          (map (partial run-solution-fn options) filtered-sols)]
    (when-not (empty? specified-missing)
      (println "Specified problems not implemented: " (join ", " specified-missing)))
    (doall time-fns)
    nil))
