(ns alexis-texas.core
  (:use com.rpl.specter)
  (:require [discljord.connections :as c]
            [discljord.messaging :as m]
            [discljord.events :as e]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.core.async :as a]
            [clojure.edn :as edn]
            [clojure.spec.alpha :as s]
            [clojure.tools.logging :as log])
  (:gen-class))

(def token (str/trim (slurp (io/resource "token.txt"))))
(def owner (str/trim (slurp (io/resource "owner.txt"))))

(def state (atom nil))

(defn- regex-cond-helper
  [expr-sym clauses default]
  (if-not (empty? clauses)
    (let [inner-clause (regex-cond-helper expr-sym (rest clauses) default)
          {:keys [regex bindings body] :as clause} (first clauses)
          regex (case (first regex)
                  :regex (second regex)
                  :else `(re-pattern ~(second regex)))
          binding (if (empty? bindings)
                    '_
                    (into `[_#] bindings))]
      `(if-let [~binding (re-find ~regex ~expr-sym)]
         (do ~@body)
         ~inner-clause))
    `(do ~@default)))

(s/def ::regex-cond-args (s/cat :expr any?
                                :clauses
                                (s/* (s/spec
                                      (s/cat :regex
                                             (s/or :regex
                                                   (partial instance? java.util.regex.Pattern)
                                                   :else any?)
                                             :bindings (s/coll-of symbol?
                                                                  :kind vector?)
                                             :body (s/* any?))))
                                :default (s/* any?)))
(defmacro regex-cond
  ""
  {:arglists '([expr]
               [expr (* [regex bindings & body])]
               [expr (* [regex bindings & body]) & default])
   :style/indent [:defn [:defn]]}
  [& args]
  (let [{:keys [expr clauses default]} (s/conform ::regex-cond-args args)
        expr-sym (gensym)]
    `(let [~expr-sym ~expr]
       ~(regex-cond-helper expr-sym clauses default))))
(s/fdef regex-cond
  :args ::regex-cond-args)

(defmulti handle-event
  ""
  (fn [event-type event-data]
    event-type))

(defmethod handle-event :default
  [event-type event-data])

(defmethod handle-event :message-create
  [event-type {{:keys [bot id]} :author :keys [content channel-id guild-id] :as event-data}]
  (try
    (when-not bot
      (let [prefix (or (select-first [ATOM :guilds guild-id :prefix] state)
                       "!")]
        (regex-cond content
                    ((str "^" prefix "disconnect") []
                     (when (= id owner)
                       (m/send-message! (:messaging @state) channel-id "Goodbye!")
                       (a/>!! (:connection @state) [:disconnect])))
                    ((str "^" prefix "quote\\s+add\\s+(\\S+)\\s+(.+)") [user q]
                     (m/send-message! (:messaging @state) channel-id
                                      (str "Adding quote to user " user))
                     (transform [ATOM :guilds guild-id :quotes user] #(conj % q) state))
                    ((str "^" prefix "quote\\s+(\\w+)") [user]
                     (let [q (rand-nth (select [ATOM :guilds guild-id :quotes user ALL] state))]
                       (m/send-message! (:messaging @state) channel-id (str user ": " q))))
                    ((str "^" prefix "quote") []
                     (let [[user quotes] (rand-nth (select [ATOM :guilds guild-id :quotes ALL]
                                                           state))
                           q (rand-nth quotes)]
                       (m/send-message! (:messaging @state) channel-id (str user ": " q))))
                    ((str "^" prefix "prefix\\s+(\\S+)") [new-prefix]
                     (m/send-message! (:messaging @state) channel-id (str "Using new prefix: "
                                                                          new-prefix))
                     (setval [ATOM :guilds guild-id :prefix] new-prefix state)))))
    (catch Exception e
      (log/error e "Exception caught in message-create handler"))))

(defmethod handle-event :disconnect
  [event-type event-data]
  (spit (io/resource "quotes.edn") (:guilds @state)))

(defn -main
  "Starts the alexis-texas bot."
  []
  (let [init-state (edn/read-string (slurp (io/resource "quotes.edn")))
        events (a/chan 100)
        connection (c/connect-bot! token events)
        messaging (m/start-connection! token)]
    (reset! state {:connection connection
                   :events events
                   :messaging messaging
                   :guilds init-state})
    (try (e/message-pump! events #'handle-event)
         (catch Exception e
           (m/stop-connection! messaging)
           (throw e)))))
