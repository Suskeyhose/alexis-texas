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
  (:import java.io.FileNotFoundException
           java.util.regex.Pattern)
  (:gen-class))

(def token (-> "token.txt"
               io/resource
               slurp
               str/trim))

(def owner (-> "owner.txt"
               io/resource
               slurp
               str/trim))

(def bot-id (-> "bot.txt"
                io/resource
                slurp
                str/trim))

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

(defmulti handle-event
  ""
  (fn [event-type event-data]
    event-type))

(defmethod handle-event :default
  [event-type event-data])

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
  {:arglists '([prefix content clauses*] [prefix content clauses* & default])
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

(defmethod handle-event :message-create
  [event-type {{:keys [bot id]} :author
               {:keys [roles]} :member
               :keys [content channel-id guild-id mentions] :as event-data}]
  (when-not bot
    (let [prefix (or (select-first [ATOM :guilds guild-id :prefix] state)
                     "!")]
      (commands prefix content
        (#"disconnect"
          (when (= id owner)
            (m/send-message! (:messaging @state) channel-id "Goodbye!")
            (c/disconnect-bot! (:connection @state))))
        (#"admin\s+add\s+(\d+)" [role-id]
          (when (or (= id owner)
                    (some (partial contains? roles)
                          (select [ATOM :guilds guild-id :admin-roles ALL] state)))
            (transform [ATOM :guilds guild-id :admin-roles]
                       #(conj % role-id) state)))
        (#"admin\s+remove\s+(\d+)" [role-id]
          (when (or (= id owner)
                    (some (partial contains? roles)
                          (select [ATOM :guilds guild-id :admin-roles ALL] state)))
            (transform [ATOM :guilds guild-id :admin-roles]
                       #(filter (partial not= role-id) %) state)))
        (#"quote\s+add\s+(\S+)\s+([\s\S]+)" [user q]
          (m/send-message! (:messaging @state) channel-id
                           (str "Adding quote to user " user))
          (transform [ATOM :guilds guild-id :quotes user] #(conj % q) state))
        (#"quote\s+remove\s+(\S+)\s+([\s\S]+)" [user q]
          (m/send-message! (:messaging @state) channel-id
                           (str "Removing quote from user " user))
          (transform [ATOM :guilds guild-id :quotes user] #(filter (partial not= q) %) state))
        (#"quote\s+(\S+)" [user]
          (let [quotes-vec (select [ATOM :guilds guild-id :quotes user ALL] state)]
            (if-not (empty? quotes-vec)
              (m/send-message! (:messaging @state) channel-id
                               (str user ": " (rand-nth quotes-vec)))
              (m/send-message! (:messaging @state) channel-id
                               (str "No quotes found for user " user "!")))))
        (#"quote\s*$"
          (let [quotes-vec (filter #(pos? (count (second %)))
                                   (select [ATOM :guilds guild-id :quotes ALL] state))]
            (if-not (empty? quotes-vec)
              (let [[user quotes] (rand-nth quotes-vec)]
                (m/send-message! (:messaging @state) channel-id
                                 (str user ": " (rand-nth quotes))))
              (m/send-message! (:messaging @state) channel-id
                               "No quotes in this server! Get to talking!"))))
        (#"prefix\s+(\S+)" [new-prefix]
          (if (or (= id owner)
                  (some (partial contains? roles)
                        (select [ATOM :guilds guild-id :admin-roles ALL] state)))
            (do (m/send-message! (:messaging @state) channel-id (str "Using new prefix: "
                                                                     new-prefix))
                (setval [ATOM :guilds guild-id :prefix] new-prefix state))
            (m/send-message! (:messaging @state) channel-id
                             "You don't have permissions to change that!")))
        (#"help"
          (display-help-message (:messaging @state) channel-id prefix))
        :default
        (when (and (= (count mentions) 1)
                   (= (:id (first mentions)) bot-id))
          (display-help-message (:messaging @state) channel-id prefix))))))

(defmethod handle-event :disconnect
  [event-type event-data]
  (log/fatal "Disconnecting from Discord.")
  (m/stop-connection! (:messaging @state))
  (swap! state assoc :running false)
  (spit "quotes.edn" (pr-str (:guilds @state))))

(defonce ^:dynamic *events* (atom nil))
(defonce ^:dynamic *connection* (atom nil))
(defonce ^:dynamic *messaging* (atom nil))

(defn -main
  "Starts the alexis-texas bot."
  []
  (let [init-state (or (try (edn/read-string (slurp "quotes.edn"))
                            (catch FileNotFoundException e
                              (log/info e "No quotes file exists, starting with empty map.")
                              nil))
                       {})
        events (reset! *events* (a/chan 100))
        connection (reset! *connection* (c/connect-bot! token events
                                                        :buffer-size 500000))
        messaging (reset! *messaging* (m/start-connection! token))]
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
    (e/message-pump! events #'handle-event)))

(defn start-bot!
  []
  (a/thread (-main)))

(defn stop-bot!
  []
  (c/disconnect-bot! @*connection*)
  (reset! *events* nil)
  (reset! *connection* nil)
  (reset! *messaging* nil))
