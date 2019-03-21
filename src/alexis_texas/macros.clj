(ns alexis-texas.macros
  (:require
   [clojure.spec.alpha :as s]))

(defn- regex-cond-helper
  [expr-sym clauses default]
  (if-not (empty? clauses)
    (let [inner-clause (regex-cond-helper expr-sym (rest clauses) default)
          {:keys [regex bindings body] :as clause} (first clauses)
          regex (case (first regex)
                  :regex (second regex)
                  :else `(re-pattern ~(second regex)))
          binding (if (empty? bindings)
                    (gensym "_")
                    (into `[_#] bindings))]
      `(if-let [~binding (re-find ~regex ~expr-sym)]
         (do ~@body)
         ~inner-clause))
    `(do ~@(second (second default)))))

(s/def ::regex-cond-clause (s/cat :regex (s/or :regex (partial instance? java.util.regex.Pattern)
                                               :else any?)
                                  :bindings (s/coll-of symbol?
                                                       :kind vector?)
                                  :body (s/* any?)))
(s/def ::regex-cond-args (s/cat :expr any?
                                :clauses (s/* (s/spec ::regex-cond-clause))
                                :default (s/? (s/cat :separator (partial = :default)
                                                     :default (s/* any?)))))
(defmacro regex-cond
  ""
  {:arglists '([expr]
               [expr clauses*]
               [expr clauses* & default])
   :style/indent [:defn [:defn]]}
  [& args]
  (let [{:keys [expr clauses default]} (s/conform ::regex-cond-args args)
        expr-sym (gensym)]
    `(let [~expr-sym ~expr]
       ~(regex-cond-helper expr-sym clauses default))))
(s/fdef regex-cond
  :args ::regex-cond-args)

(s/def ::commands-clause (s/cat :command (s/alt :string string?
                                                :regex (partial instance? java.util.regex.Pattern))
                                :bindings (s/? (s/coll-of symbol?
                                                          :kind vector?))
                                :body (s/* any?)))
(s/def ::commands-args (s/cat :prefix any?
                              :content any?
                              :clauses (s/* (s/spec ::commands-clause))
                              :default (s/? (s/cat :separator (partial = :default)
                                                   :body (s/* any?)))))
(defmacro commands
  ""
  {:arglists '([prefix content clauses*] [prefix content clauses* :default & default])
   :style/indent [:defn [:defn]]}
  [& args]
  (let [{:keys [prefix content clauses default]} (s/conform ::commands-args args)
        pfx (gensym)
        clauses (for [clause clauses]
                  `((str "^" ~pfx ~(second (:command clause))) ~(if (:bindings clause)
                                                                  (:bindings clause)
                                                                  [])
                    ~@(:body clause)))]
    `(let [~pfx (Pattern/quote ~prefix)]
       (regex-cond ~content
         ~@clauses
         ~@(when-let [body (:body default)]
             `(:default ~@body))))))
(s/fdef commands
  :args ::commands-args)

(s/def ::command-fn-clause (s/cat :command (s/alt :string string?
                                                  :regex (partial instance? java.util.regex.Pattern))
                                  :fn any?))
(s/def ::command-fns-args (s/cat :event-data any?
                                 :prefix any?
                                 :content any?
                                 :clauses (s/* (s/spec ::command-fn-clause))
                                 :default (s/? (s/cat :separator (partial = :default)
                                                      :body (s/* any?)))))

(defn- command-fns-helper
  [event-data prefix content clauses default]
  (if (seq clauses)
    (let [clause (first clauses)]
      `(if-let [args# (re-find (re-pattern (str "^" ~prefix ~(second (:command clause)))) ~content)]
         (if (string? args#)
           (~(:fn clause) ~event-data)
           (apply ~(:fn clause) ~event-data (rest args#)))
         ~(command-fns-helper event-data prefix content (rest clauses) default)))
    `(do ~@(:body default))))

(defmacro command-fns
  ""
  {:arglists '([prefix content clauses*] [prefix content clauses* :default & default])
   :style/indent [:defn]}
  [& args]
  (let [{:keys [event-data prefix content clauses default]} (s/conform ::command-fns-args args)
        pfx (gensym)
        cntnt (gensym)]
    `(let [~pfx (Pattern/quote ~prefix)
           ~cntnt ~content]
       ~(command-fns-helper event-data pfx cntnt clauses default))))
(s/fdef command-fns
  :args ::command-fns-args)
