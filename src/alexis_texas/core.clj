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
  (:import java.io.FileNotFoundException)
  (:gen-class))

(def token (str/trim (slurp (io/resource "token.txt"))))
(def owner (str/trim (slurp (io/resource "owner.txt"))))
(def bot-id (str/trim (slurp (io/resource "bot.txt"))))

(defonce state (atom nil))

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
    `(do ~@(second (second default)))))

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
                                :default (s/? (s/cat :separator (partial = :default)
                                                     :default (s/* any?)))))
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

(defn cleanse-regex
  "Takes in a string, and outputs a string with the appropriate escapes so that it doesn't break."
  [s]
  s)

(defn display-help-message
  "Takes a core.async channel for communicating with the messaging process,
  the channel ID on which to send a help message, and the current guild's prefix,
  and sends a help message to Discord."
  [messaging channel-id prefix]
  (m/send-message! messaging channel-id
                   (str "Hi! I'm Alexis Texas, a Discord Bot meant to help you keep"
                        " track of funny and interesting quotes in your server.\n\n"

                        "I only have a couple of commands to remember.\n"
                        "`" prefix "help` displays this message!\n"
                        "`" prefix "quote` will get a random quote from your server's list.\n"
                        "`" prefix "quote <name>` will get a random quote said by the"
                        " named user.\n"
                        "`" prefix "quote add <name> <quote>` will add a new quote"
                        " to your server.\n"
                        "`" prefix "quote remove <name> <quote>` removes the given quote."
                        " Later this will be done by an ID instead of the copied and"
                        " pasted quote.\n\n"

                        "If you are an admin on the server, feel free to make"
                        " use of these additional commands:\n"
                        "`" prefix "prefix <new prefix>` -- NOT YET IMPLEMENTED\n")))

(defmethod handle-event :message-create
  [event-type {{:keys [bot id]} :author
               {:keys [roles]} :member
               :keys [content channel-id guild-id mentions] :as event-data}]
  (try
    (when-not bot
      (let [prefix (or (select-first [ATOM :guilds guild-id :prefix] state)
                       "!")]
        (regex-cond content
          ((str "^" prefix "disconnect") []
           (when (= id owner)
             (m/send-message! (:messaging @state) channel-id "Goodbye!")
             (a/>!! (:connection @state) [:disconnect])))
          ((str "^" prefix "admin\\s+add\\s+(\\d+)") [role-id]
           (when (or (= id owner)
                     (some (partial contains? roles)
                           (select [ATOM :guilds guild-id :admin-roles ALL] state)))
             (transform [ATOM :guilds guild-id :admin-roles]
                        #(conj % role-id) state)))
          ((str "^" prefix "admin\\s+remove\\s+(\\d+)") [role-id]
           (when (or (= id owner)
                     (some (partial contains? roles)
                           (select [ATOM :guilds guild-id :admin-roles ALL] state)))
             (transform [ATOM :guilds guild-id :admin-roles]
                        #(filter (partial not= role-id) %) state)))
          ((str "^" prefix "quote\\s+add\\s+(\\S+)\\s+(.+)") [user q]
           (m/send-message! (:messaging @state) channel-id
                            (str "Adding quote to user " user))
           (transform [ATOM :guilds guild-id :quotes user] #(conj % q) state))
          ((str "^" prefix "quote\\s+remove\\s+(\\S+)\\s+(.+)") [user q]
           (m/send-message! (:messaging @state) channel-id
                            (str "Removing quote from user " user))
           (transform [ATOM :guilds guild-id :quotes user] #(filter (partial not= q) %) state))
          ((str "^" prefix "quote\\s+(\\w+)") [user]
           (let [quotes-vec (select [ATOM :guilds guild-id :quotes user ALL] state)]
             (if-not (empty? quotes-vec)
               (m/send-message! (:messaging @state) channel-id
                                (str user ": " (rand-nth quotes-vec)))
               (m/send-message! (:messaging @state) channel-id
                                (str "No quotes found for user " user "!")))))
          ((str "^" prefix "quote\\s+$") []
           (let [quotes-vec (filter #(pos? (count (second %)))
                                    (select [ATOM :guilds guild-id :quotes ALL] state))]
             (if-not (empty? quotes-vec)
               (let [[user quotes] (rand-nth quotes-vec)]
                 (m/send-message! (:messaging @state) channel-id
                                  (str user ": " (rand-nth quotes))))
               (m/send-message! (:messaging @state) channel-id
                                "No quotes in this server! Get to talking!"))))
          ((str "^" prefix "prefix\\s+(\\S+)") [new-prefix]
           (if (or (= id owner)
                   (some (partial contains? roles)
                         (select [ATOM :guilds guild-id :admin-roles ALL] state)))
             (do (m/send-message! (:messaging @state) channel-id (str "Using new prefix: "
                                                                      new-prefix))
                 (setval [ATOM :guilds guild-id :prefix] (cleanse-regex new-prefix) state))
             (m/send-message! (:messaging @state) channel-id
                              "You don't have permissions to change that!")))
          ((str "^" prefix "help") []
           (display-help-message (:messaging @state) channel-id
                                 (select-first [ATOM :guilds guild-id :prefix] state)))
          :default
          (when (and (= (count mentions) 1)
                     (= (:id (first mentions)) bot-id))
            (display-help-message (:messaging @state) channel-id
                                  (select-first [ATOM :guilds guild-id :prefix] state))))))
    (catch Exception e
      (log/error e "Exception caught in message-create handler"))))

(defmethod handle-event :disconnect
  [event-type event-data]
  (spit "quotes.edn" (pr-str (:guilds @state))))

(defn -main
  "Starts the alexis-texas bot."
  []
  (let [init-state (or (try (edn/read-string (slurp "quotes.edn"))
                            (catch FileNotFoundException e
                              (log/info e "No quotes file exists, starting with empty map.")
                              nil))
                       {})
        events (a/chan 100)
        connection (c/connect-bot! token events)
        messaging (m/start-connection! token)]
    (reset! state {:connection connection
                   :events events
                   :messaging messaging
                   :guilds init-state
                   :running true})
    (a/go-loop []
      (a/<! (a/timeout 300000))
      (spit "quotes.edn" (pr-str (:guilds @state)))
      (when (:running @state)
        (recur)))
    (try (e/message-pump! events #'handle-event)
         (catch Exception e
           (swap! state assoc :running false)
           (throw e)))
    (m/stop-connection! messaging))
  (shutdown-agents))
