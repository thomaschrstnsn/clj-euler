(ns clj-euler.runner
  (:use [clj-euler utils find-solutions])
  (:use [clojure.set :only [difference]])
  (:use [clojure.string :only [join]])
  (:use [clansi.core :only [style]]))

(defn- exception-wrap [fn]
  (try (fn)
       (catch Exception e
         (str "Caught exception " (.getMessage e)))))

(defn- run-solution-fn [{:keys [timing dryrun verify]}
                        {:keys [fn problem-num expected arguments pure-fn metapath]}]
  (let [timer (if timing
                #(time (%))
                #(%))
        fname (:name (meta pure-fn))]
    (println (str (style "[" :white)
                  (style (str "problem ") :cyan)
                  (style (str problem-num) :yellow)
                  (style " / " :white)
                  (style (str (name metapath)) :cyan)
                  (style "] " :white)
                  (style fname :yellow)))
    (when-not dryrun
      (let [result      (timer (partial exception-wrap fn))
            noexpected? (and verify
                             (nil? expected))
            unexpected? (and verify
                             ((complement nil?) expected)
                             (not= result expected))]
        (println
         (str
          (when-not (or unexpected? noexpected?)
            (style (str "=== " result) :green))
          (when unexpected?
            (str (style "!!!" :black :bg-red)
                 (style (str " " result " was not expected: " expected) :red)))
          (when noexpected?
            (str (style "???" :bg-yellow :black)
                 (style (str " " result " (no expected result)") :yellow)))
          "\n"))))))
(defn run [{:keys [metapaths problem-set all] :as options}]
  {:pre [((complement empty?) metapaths)]}
  (let [all-solutions     (solutions-from-namespaces metapaths)
        specified-missing (difference problem-set
                                      (set (map :problem-num all-solutions)))

        num-predicate     (if all
                            (fn [n] true)
                            (fn [n] (contains? problem-set n)))

        filtered-sols     (filter (comp num-predicate :problem-num) all-solutions)
        ordered-sols      (sort-by #(vec (map % [:problem-num :metapath])) filtered-sols)
        time-fns          (map (partial run-solution-fn options) ordered-sols)]
    (when-not (empty? specified-missing)
      (println "Specified problems not implemented: " (join ", " specified-missing)))
    (doall time-fns)
    nil))
