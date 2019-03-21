(ns alexis-texas.commands
  (:use
   com.rpl.specter)
  (:require
   [alexis-texas.blacklist :as blacklist]
   [alexis-texas.events :refer [state]]
   [alexis-texas.macros :refer [commands command-fns]]
   [alexis-texas.mafia.commands :as mafia.c]
   [alexis-texas.permissions :refer [user-has-permission?]]
   [alexis-texas.util :refer [owner]]
   [clojure.pprint :refer [pprint]]
   [clojure.string :as str]
   [taoensso.timbre :as log]
   [discljord.connections :as c]
   [discljord.messaging :as m])
  (:import
   (java.util.regex Pattern)))

(defn help-message
  "Takes a core.async channel for communicating with the messaging process,
  the channel ID on which to send a help message, and the current guild's prefix,
  and sends a help message to Discord."
  [prefix admin?]
  (str "Hi! I'm " (:bot-name @state) ", a Discord Bot meant to help you keep"
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

       "Pretty soon I will add playing the game Mafia to my features. If"
       " you'd like to see how to play, use this command:\n"
       "`" prefix "mafia help`\n\n"
       (when admin?
         (str "Since you are an admin on the server, feel free to make"
              " use of these additional commands:\n"
              "`" prefix "prefix <new prefix>` changes the bot's prefix for this server.\n"
              "`" prefix "blacklist add <name>` bans members joining the server with this in"
              " their name.\n"
              "`" prefix "blacklist add regex <regex>` bans members joining the server with "
              "names that match the regular expression.\n"
              "`" prefix "blacklist remove <listed number>` removes the blacklist name or "
              "pattern based on its index as shown by the list command.\n"
              "`" prefix "blacklist list` lists all the names and patterns"
              " blacklisted by this bot.\n"
              "`" prefix "blacklist clear` clears the blacklist of both names and"
              " patterns.\n"))))


(defn- disconnect
  [{:keys [channel-id] {id :id} :author}]
  (when (= id owner)
    (m/create-message! (:messaging @state) channel-id :content "Goodbye!")
    (c/disconnect-bot! (:connection @state))))

(defn- add-quote
  [{:keys [channel-id guild-id]} user q]
  (m/create-message! (:messaging @state) channel-id
                     :content (str "Adding quote to user " user))
  (transform [ATOM :state (keypath guild-id) :quotes (keypath user)] #(conj (or % []) q) state))

(defn- remove-quote
  [{:keys [channel-id guild-id]} user q]
  (m/create-message! (:messaging @state) channel-id
                     :content (str "Removing quote from user " user))
  (transform [ATOM :state (keypath guild-id) :quotes (keypath user)]
             #(filter (partial not= q) %)
             state))

(defn- quote-for-user
  [{:keys [guild-id channel-id]} user]
  (let [quotes-vec (select [ATOM :state (keypath guild-id) :quotes (keypath user) ALL] state)]
    (if-not (empty? quotes-vec)
      (m/create-message! (:messaging @state) channel-id
                         :content (str user ": " (rand-nth quotes-vec)))
      (m/create-message! (:messaging @state) channel-id
                         :content (str "No quotes found for user " user "!")))))

(defn- rand-quote
  [{:keys [guild-id channel-id]}]
  (let [quotes-vec (filter #(pos? (count (second %)))
                           (select [ATOM :state (keypath guild-id) :quotes ALL] state))]
    (if-not (empty? quotes-vec)
      (let [[user quotes] (rand-nth quotes-vec)]
        (m/create-message! (:messaging @state) channel-id
                           :content (str user ": " (rand-nth quotes))))
      (m/create-message! (:messaging @state) channel-id
                         :content "No quotes in this server! Get to talking!"))))

(defn- create-new-prefix
  [{:keys [guild-id channel-id] {id :id} :author} new-prefix]
  (let [admin? (or (= id owner)
                   (= id (select-any [ATOM :guilds (keypath guild-id) :owner-id] state))
                   (user-has-permission? id guild-id :manage-guild))]
    (if admin?
      (do (m/create-message! (:messaging @state) channel-id
                             :content (str "Using new prefix: " new-prefix))
          (setval [ATOM :state (keypath guild-id) :prefix] new-prefix state))
      (m/create-message! (:messaging @state) channel-id
                         :content "You don't have permissions to change that!"))))

(defn- ping
  [{:keys [channel-id]}]
  (m/create-message! (:messaging @state) channel-id :content "pong!"))

(defn process-message
  [{:keys [mentions content webhook-id guild-id channel-id] {bot :bot id :id} :author :as event-data}]
  (when-not (or bot webhook-id)
    (let [prefix (or (select-first [ATOM :state (keypath guild-id) :prefix] state)
                     "!")]
      (command-fns event-data prefix content
        ;; debugging
        (#"ping" #'ping)
        (#"mafia\s+phase\s+next" #'mafia.c/mafia-phase-next)

        ;; author commands
        (#"disconnect" #'disconnect)

        ;; quote stuff
        (#"quote\s+add\s+(\S+)\s+([\s\S]+)" #'add-quote)
        (#"quote\s+remove\s+(\S+)\s+([\s\S]+)" #'remove-quote)
        (#"quote\s+(\S+)" #'quote-for-user)
        (#"quote\s*$" #'rand-quote)

        ;; mafia commands
        (#"mafia\s+help" #'mafia.c/mafia-help)
        (#"mafia\s+start" #'mafia.c/mafia-start)
        (#"mafia\s+join" #'mafia.c/mafia-join)
        (#"mafia\s+leave" #'mafia.c/mafia-leave)
        (#"mafia\s+stop" #'mafia.c/mafia-stop)
        (#"mafia" #'mafia.c/invalid-mafia-command)

        ;; admin commands
        (#"prefix\s+(\S+)" #'create-new-prefix)
        (#"blacklist\s+add\s+regex\s+([\S\s]+)" #'blacklist/add-blacklist-regex)
        (#"blacklist\s+add\s+([\S\s]+)" #'blacklist/add-blacklist-string)
        (#"blacklist\s+list" #'blacklist/list-blacklist)
        (#"blacklist\s+clear" #'blacklist/clear-blacklist)

        ;; general
        (#"help" #'send-help-message)
        :default
        (when (and (= (count mentions) 1)
                   (= (:id (first mentions)) (:bot-id @state)))
          (let [admin? (or (= id owner)
                           (= id (select-any [ATOM :guilds (keypath guild-id) :owner-id] state))
                           (user-has-permission? id guild-id :manage-guild))]
            (m/create-message! (:messaging @state) channel-id :content (help-message prefix admin?))))))))
