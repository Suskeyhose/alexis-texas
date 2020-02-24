(ns alexis-texas.commands
  (:use
   com.rpl.specter)
  (:require
   [alexis-texas.blacklist :as blacklist]
   [alexis-texas.events :refer [state]]
   [alexis-texas.introductions :as intro]
   [alexis-texas.macros :refer [command-fns]]
   [alexis-texas.mafia.commands :as mafia.c]
   [alexis-texas.permissions :refer [user-has-permission? admin?]]
   [alexis-texas.prune :refer [prune-list]]
   [alexis-texas.quotes :as quotes]
   [alexis-texas.state :refer [get-prefix]]
   [alexis-texas.util :refer [owner]]
   [clojure.pprint :refer [pprint]]
   [clojure.string :as str]
   [taoensso.timbre :as log]
   [discljord.connections :as c]
   [discljord.messaging :as m]
   [clojure.core.async :as a]))

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
              " patterns.\n"
              "`" prefix "prune list` posts a ping for, the username of, and id"
              " of each user who has not posted a message in any channel on the server in"
              " a number of days set by `prune duration`.\n"
              "`" prefix "prune duration <days of inactivity>` sets the number of days of"
              " inactivity required to be included in a prune listing.\n"
              "\n"
              "I also have features for handling introductions, try the following command if "
              "you want more information about those features.\n"
              "`" prefix "introduction help`"))))

(defn send-help-message
  [{:keys [channel-id guild-id author]}]
  (let [prefix (get-prefix state guild-id)]
    (m/create-message! (:messaging @state) channel-id :content (help-message prefix (admin? guild-id author)))))

(defn disconnect
  [{:keys [channel-id] {id :id} :author}]
  (when (= id owner)
    (m/create-message! (:messaging @state) channel-id :content "Goodbye!")
    (c/disconnect-bot! (:connection @state))))

(defn create-new-prefix
  [{:keys [guild-id channel-id author]} new-prefix]
  (if (admin? guild-id author)
    (do (m/create-message! (:messaging @state) channel-id
                           :content (str "Using new prefix: " new-prefix))
        (setval [ATOM :state (keypath guild-id) :prefix] new-prefix state))
    (m/create-message! (:messaging @state) channel-id
                       :content "You don't have permissions to change that!")))

(defn ping
  [{:keys [channel-id] {id :id} :author}]
  (when (= id owner)
    (m/create-message! (:messaging @state) channel-id :content "pong!")))

(defn get-prune-list
  [{:keys [guild-id channel-id author]}]
  (log/debug "Starting a prune!")
  (m/create-message! (:messaging @state) channel-id
                     :content "Starting a prune listing!")
  (a/thread
    (try
      (when (admin? guild-id author)
        (log/debug "Arguments to prune-list: guild-id " guild-id
                   " prune duration " (select-one [ATOM :state (keypath guild-id)
                                                   :prune-duration (nil->val 90)]
                                                  state))
        (let [prune-list-results (prune-list @state channel-id guild-id
                                             (select-one [ATOM :state (keypath guild-id)
                                                          :prune-duration (nil->val 90)]
                                                         state))
              user-lines (map #(let [user (select-one [ATOM :users (keypath %)] state)
                                     nick (str (:username user)
                                               "#"
                                               (:discriminator user))
                                     ping (str "<@" % ">")]
                                 (str ping "\t\t\t**Username:** " nick "\t\t\t**User ID:** " % "\n"))
                              prune-list-results)
              send-list (partition-by (let [num-chars (volatile! 0)
                                            msg-idx (volatile! 0)]
                                        (fn [line]
                                          (vswap! num-chars #(+ % (count line)))
                                          (when (> @num-chars 2000)
                                            (vswap! msg-idx inc)
                                            (vreset! num-chars 0))
                                          @msg-idx))
                                      user-lines)]
          (if (zero? (count send-list))
            (m/create-message! (:messaging @state) channel-id
                               :content "Nobody to prune!")
            (doseq [msg send-list]
              (m/create-message! (:messaging @state) channel-id
                                 :content (apply str msg))))))
      (catch Exception e
        (m/create-message! (:messaging @state) channel-id
                           :content (str "Failed to do a prune listing. Error encountered!\n"
                                         (.getMessage e)))
        (log/error e)))))

(defn set-prune-duration
  [{:keys [guild-id channel-id]} duration]
  (setval [ATOM :state (keypath guild-id) :prune-duration] (Long/parseLong duration) state)
  (m/create-message! (:messaging @state) channel-id
                     :content (str "Setting the prune duration to " duration " days!")))

(defn display-prune-duration
  [{:keys [guild-id channel-id]}]
  (m/create-message! (:messaging @state) channel-id
                     :content (str "The prune duration is currently: "
                                   (select-one [ATOM :state (keypath guild-id) :prune-duration (nil->val 90)]
                                               state)
                                   " days.")))

(defn process-message
  [{:keys [mentions content webhook-id guild-id channel-id]
    {user-id :id bot :bot :as author} :author :as event-data}]
  (when-not (or bot webhook-id)
    (let [prefix (get-prefix state guild-id)]
      (command-fns event-data prefix content
        ;; debugging
        (#"ping" #'ping)

        ;; author commands
        (#"disconnect" #'disconnect)

        ;; quote stuff
        (#"quote\s+add\s+(\S+)\s+([\s\S]+)" #'quotes/add-quote)
        (#"quote\s+remove\s+(\S+)\s+([\s\S]+)" #'quotes/remove-quote)
        (#"quote\s+(\S+)" #'quotes/quote-for-user)
        (#"quote\s*$" #'quotes/rand-quote)

        ;; mafia commands
        (#"mafia\s+help" #'mafia.c/mafia-help)
        ;; (#"mafia\s+start" #'mafia.c/mafia-start)
        ;; (#"mafia\s+phase\s+next" #'mafia.c/mafia-phase-next)
        ;; (#"mafia\s+phase\s+current" #'mafia.c/mafia-phase-current)
        ;; (#"mafia\s+state" #'mafia.c/mafia-state)
        ;; (#"mafia\s+stop" #'mafia.c/mafia-stop)

        ;; admin commands
        (#"prefix\s+(\S+)" #'create-new-prefix)
        (#"blacklist\s+add\s+regex\s+([\S\s]+)" #'blacklist/add-blacklist-regex)
        (#"blacklist\s+add\s+([\S\s]+)" #'blacklist/add-blacklist-string)
        (#"blacklist\s+list" #'blacklist/list-blacklist)
        (#"blacklist\s+clear" #'blacklist/clear-blacklist)
        (#"prune\s+list" #'get-prune-list)
        (#"prune\s+duration\s+(\d+)" #'set-prune-duration)
        (#"prune\s+duration" #'display-prune-duration)

        ;; introduction commands
        (#"introduction\s+channel\s+<#(\d+)>" #'intro/set-channel)
        (#"introduction\s+channel\s+(\d+)" #'intro/set-channel)
        (#"introduction\s+channel" #'intro/get-channel)
        (#"introduction\s+member\s+<@&(\d+)>" #'intro/set-member-role)
        (#"introduction\s+member\s+(\d+)" #'intro/set-member-role)
        (#"introduction\s+member" #'intro/get-member-role)
        (#"introduction\s+guest\s+<@&(\d+)>" #'intro/set-guest-role)
        (#"introduction\s+guest\s+(\d+)" #'intro/set-guest-role)
        (#"introduction\s+guest" #'intro/get-guest-role)
        (#"introduction\s+length\s+(\d+)" #'intro/set-minimum-length)
        (#"introduction\s+length" #'intro/get-minimum-length)
        (#"introduction\s+pattern\s+([\s\S]+)" #'intro/add-regex-pattern)
        (#"introduction\s+text\s+([\s\S]+)" #'intro/add-string-pattern)
        (#"introduction\s+patterns" #'intro/list-patterns)
        (#"introduction\s+delete\s+(\d+)" #'intro/delete-pattern)
        (#"introduction\s+reset" #'intro/reset-state)
        (#"introduction\s+help" (fn [{:keys [channel-id guild-id] {author-id :id} :author}]
                                  (when (admin? guild-id author-id)
                                    (m/create-message! (:messaging @state) channel-id
                                                       :content (#'intro/help-message prefix)))))

        ;; general
        (#"help" #'send-help-message)
        :default
        (if (and (= (count mentions) 1)
                 (= (:id (first mentions)) (:bot-id @state))
                 (re-matches #"^<@!?\d+>$" content)
                 (not= (intro/intro-channel guild-id)
                       channel-id))
          (m/create-message! (:messaging @state) channel-id
                             :content (help-message prefix (admin? guild-id author)))
          (if (= (intro/intro-channel guild-id)
                 channel-id)
            (when (intro/valid-intro? guild-id content)
              (intro/give-roles guild-id user-id))
            (mafia.c/process-state-commands event-data)))))))
