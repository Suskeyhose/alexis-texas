(ns alexis-texas.commands
  (:use
   com.rpl.specter)
  (:require
   [alexis-texas.events :refer [state]]
   [alexis-texas.macros :refer [commands]]
   [alexis-texas.mafia :as mafia]
   [alexis-texas.mafia.state :as mafia.s]
   [alexis-texas.permissions :refer [user-has-permission?]]
   [alexis-texas.util :refer [resource]]
   [clojure.string :as str]
   [clojure.tools.logging :as log]
   [discljord.connections :as c]
   [discljord.messaging :as m])
  (:import
   (java.util.regex Pattern)))

(defn help-message
  "Takes a core.async channel for communicating with the messaging process,
  the channel ID on which to send a help message, and the current guild's prefix,
  and sends a help message to Discord."
  [prefix admin?]
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

(defn mafia-help-message
  ""
  [prefix admin?]
  (str "Coming soon:tm:\n\n"
       "Welcome to Mafia!\n\n"
       "Mafia is a game of social intrigue. Players who join will be randomly assigned"
       " to being a villager, a member of the mafia, a medic, or an investigator."
       " While playing the game, each round of play is represented as a night, and"
       " the following day.\n\n"

       "During the night the Mafia can try to kill non-Mafia, Medics can select"
       " someone they would like to save from the Mafia, should they try to kill them,"
       " and the Investigator can learn the role of someone else.\n"
       "During the day, the players who were killed in the night are revealed and they get"
       " to say their last words. Then the remaining players get to vote for who they"
       " think is a mafia member. The player with the most votes says their last words"
       " and then is hanged, then the whole process starts over again.\n"
       "The game is over once either all of the Mafia are dead, or everyone else is."
       " Obviously the survivors are the winners.\n\n"

       "Commands:\n"
       "`" prefix "mafia start` \n"
       "`" prefix "mafia join` \n"
       "`" prefix "mafia leave` \n"
       (when admin?
         (str "Admins also have access to the following:\n"
          "`" prefix "mafia stop` \n"))
       "\n"
       "Once the game is started, many parts of the game have to take place in private."
       " As a result, the bot will have to send you private messages while you play."
       " That means you will have to allow DMs from server members (which is on by default)."
       " Any additional instructions that are needed will be given to you when needed."))

(def owner (resource "owner.txt"))
(def bot-id (resource "bot.txt"))

(defn process-message
  [{{:keys [bot id]} :author
    {:keys [roles]} :member
    :keys [content channel-id guild-id mentions webhook-id] :as event-data}]
  (when-not (or bot webhook-id)
    (let [prefix (or (select-first [ATOM :state (keypath guild-id) :prefix] state)
                     "!")
          admin? (or (= id owner)
                     (= id (select-any [ATOM :guilds (keypath guild-id) :owner-id] state))
                     (user-has-permission? id guild-id :manage-guild))]
      (commands prefix content
        ;; Owner commands
        (#"disconnect"
          (when (= id owner)
            (m/send-message! (:messaging @state) channel-id "Goodbye!")
            (c/disconnect-bot! (:connection @state))))

        ;; Quote commands
        (#"quote\s+add\s+(\S+)\s+([\s\S]+)" [user q]
          (m/send-message! (:messaging @state) channel-id
                           (str "Adding quote to user " user))
          (transform [ATOM :state (keypath guild-id) :quotes (keypath user)] #(conj (or % []) q) state))
        (#"quote\s+remove\s+(\S+)\s+([\s\S]+)" [user q]
          (m/send-message! (:messaging @state) channel-id
                           (str "Removing quote from user " user))
          (transform [ATOM :state (keypath guild-id) :quotes (keypath user)]
                     #(filter (partial not= q) %)
                     state))
        (#"quote\s+(\S+)" [user]
          (let [quotes-vec (select [ATOM :state (keypath guild-id) :quotes (keypath user) ALL] state)]
            (if-not (empty? quotes-vec)
              (m/send-message! (:messaging @state) channel-id
                               (str user ": " (rand-nth quotes-vec)))
              (m/send-message! (:messaging @state) channel-id
                               (str "No quotes found for user " user "!")))))
        (#"quote\s*$"
          (let [quotes-vec (filter #(pos? (count (second %)))
                                   (select [ATOM :state (keypath guild-id) :quotes ALL] state))]
            (if-not (empty? quotes-vec)
              (let [[user quotes] (rand-nth quotes-vec)]
                (m/send-message! (:messaging @state) channel-id
                                 (str user ": " (rand-nth quotes))))
              (m/send-message! (:messaging @state) channel-id
                               "No quotes in this server! Get to talking!"))))

        ;; Mafia commands
        (#"mafia\s+help"
          (m/send-message! (:messaging @state) channel-id
                           (mafia-help-message prefix admin?)))
        (#"mafia\s+start"
          (when-not (mafia.s/active-game? state guild-id)
              (m/send-message! (:messaging @state) channel-id
                               (str "Soon:tm:: Starting new mafia game"))))
        (#"mafia\s+join"
          (m/send-message! (:messaging @state) channel-id
                           (str "Soon:tm:: User " (:username ((:users @state) id)) " joined the mafia game!")))
        (#"mafia\s+leave"
          (m/send-message! (:messaging @state) channel-id
                           (str "Soon:tm:: User " (:username ((:users @state) id)) " left the mafia game.")))
        (#"mafia\s+stop"
          (when admin?
            (m/send-message! (:messaging @state) channel-id
                             (str "Soon:tm:: Stopping the mafia game!"))))

        ;; Admin commands
        (#"prefix\s+(\S+)" [new-prefix]
          (if admin?
            (do (m/send-message! (:messaging @state) channel-id
                                 (str "Using new prefix: " new-prefix))
                (setval [ATOM :state guild-id :prefix] new-prefix state))
            (m/send-message! (:messaging @state) channel-id
                             "You don't have permissions to change that!")))
        (#"blacklist\s+add\s+regex\s+([\S\s]+)" [blacklist-item]
          (when admin?
            (transform [ATOM :state (keypath guild-id) :blacklist]
                       #(conj (or % []) (re-pattern blacklist-item))
                       state)
            (m/send-message! (:messaging @state) channel-id
                             "Adding new blacklisted pattern")))
        (#"blacklist\s+add\s+([\S\s]+)" [blacklist-item]
          (when admin?
            (transform [ATOM :state (keypath guild-id) :blacklist]
                       #(conj (or % []) blacklist-item)
                       state)
            (m/send-message! (:messaging @state) channel-id
                             "Adding new blacklisted name")))
        (#"blacklist\s+list"
          (when admin?
            (m/send-message! (:messaging @state) channel-id
                             (apply str "Blacklisted names:\n"
                                    (interpose
                                     "\n"
                                     (map-indexed
                                      #(str %1 ": " %2)
                                      (select [ATOM :state (keypath guild-id) :blacklist ALL]
                                              state)))))))
        (#"blacklist\s+clear"
          (when admin?
            (setval [ATOM :state guild-id :blacklist ALL] NONE state)
            (m/send-message! (:messaging @state) channel-id
                             "Clearing the blacklist!")))
        (#"blacklist\s+remove\s+(\d+)" [idx]
          (when admin?
            (let [num (Long/parseLong idx)
                  item (select-first [ATOM :state (keypath guild-id) :blacklist (keypath num)] state)]
              (setval [ATOM :state guild-id :blacklist num] NONE state)
              (m/send-message! (:messaging @state) channel-id
                               (str "Removing blacklist item: "
                                    item)))))

        ;; Get help
        (#"help"
          (m/send-message! (:messaging @state) channel-id (help-message prefix admin?)))
        :default
        (when (and (= (count mentions) 1)
                   (= (:id (first mentions)) bot-id))
          (m/send-message! (:messaging @state) channel-id (help-message prefix admin?)))))))
