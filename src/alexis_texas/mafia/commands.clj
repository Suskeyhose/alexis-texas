(ns alexis-texas.mafia.commands
  (:use
   com.rpl.specter)
  (:require
   [alexis-texas.events :refer [state]]
   [alexis-texas.macros :refer [command-fns]]
   [alexis-texas.mafia :as mafia]
   [alexis-texas.mafia.state :as mafia.s]
   [alexis-texas.permissions :refer [user-has-permission? admin?]]
   [alexis-texas.util :refer [owner]]
   [clojure.pprint :as pprint]
   [discljord.messaging :as m]))

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
       (when admin?
         (str "Admins also have access to the following:\n"
          "`" prefix "mafia stop` \n"))
       "\n"
       "Once the game is started, many parts of the game have to take place in private."
       " As a result, the bot will have to send you private messages while you play."
       " That means you will have to allow DMs from server members (which is on by default)."
       " Any additional instructions that are needed will be given to you when needed."))

(defn mafia-help
  [{:keys [guild-id channel-id author]}]
  ;; TODO: Make this state-dependant for the guild
  (let [prefix (or (select-first [ATOM :state (keypath guild-id) :prefix] state)
                   "!")]
    (m/create-message! (:messaging @state) channel-id
                       :content (mafia-help-message prefix (admin? guild-id author)))))

(defn mafia-start
  [{:keys [guild-id channel-id]}]
  (let [game-state (mafia.s/game-state state guild-id)]
    (if-not (:playing? game-state)
      (do (m/create-message! (:messaging @state) channel-id
                             :content (str "Starting new mafia game"))
          (mafia.s/start-game! state guild-id channel-id))
      (m/create-message! (:messaging @state) channel-id
                         :content (str "A game has already been started in this guild!"
                                       " Check out <#"
                                       (:channel game-state)
                                       "> for more information!")))))

(defn mafia-join
  "Adds the author of the message to the state "
  [{:keys [guild-id channel-id] {id :id} :author}]
  (let [conn (:messaging @state)]
    (if (contains? (:state (mafia.s/game-state state guild-id))
                   id)
      (m/create-message!
       conn channel-id
       :content
       "You've already joined this game!")
      (do
        (mafia.s/update-game-state state guild-id
                                   (fn [{:keys [state] :as game-state}]
                                     (assoc game-state
                                            :state (conj state id))))
        (let [game-state (mafia.s/game-state state guild-id)
              game-channel (:channel game-state)]
          (when-not (= channel-id game-channel)
            (m/create-message!
             conn game-channel
             :content
             (str (select-first [ATOM :users (keypath id) :username] state)
                  " joined the mafia game!")))
          (m/create-message!
           conn channel-id
           :content "Joined the mafia game!"))))))

(defn mafia-leave
  [{:keys [guild-id channel-id] {id :id} :author}]
  (let [game-state (mafia.s/game-state state guild-id)
        conn (:messaging @state)]
    (when (contains? (:state game-state)
                     id)
      (mafia.s/update-game-state state guild-id
                                 (fn [{:keys [state] :as game-state}]
                                   (assoc game-state
                                          :state (disj state id))))
      (let [game-channel (:channel game-state)]
        (when-not (= channel-id game-channel)
          (m/create-message!
           conn game-channel
           :content
           (str (select-first [ATOM :users (keypath id) :username] state)
                " left the game!"))))
      (m/create-message!
       conn channel-id
       :content "Left the mafia game!"))))

(defn mafia-stop
  [{:keys [guild-id channel-id author]}]
  (when (admin? guild-id author)
    (m/create-message!
     (:messaging @state) channel-id
     :content
     (if (:playing? (mafia.s/game-state state guild-id))
       (do (mafia.s/stop-game! state guild-id)
           (str "Stopping the mafia game!"))
       (str "No game is currently running.")))))

(defn mafia-phase-next
  [{:keys [guild-id channel-id author]}]
  (when (and (admin? guild-id author)
             (:playing? (mafia.s/game-state state guild-id)))
    (mafia.s/advance-phase state guild-id)
    (m/create-message! (:messaging @state) channel-id
                       :content (str "Advancing game to next phase"))))

(defn mafia-phase-current
  [{:keys [guild-id channel-id author]}]
  (when (admin? guild-id author)
    (m/create-message! (:messaging @state) channel-id
                       :content
                       (if (:playing? (mafia.s/game-state state guild-id))
                         (str "Current phase is "
                              (:phase (mafia.s/game-state state guild-id)))
                         "No game is currently being played in this guild."))))

(defn mafia-state
  [{:keys [guild-id channel-id author]}]
  (when (admin? guild-id author)
    (m/create-message! (:messaging @state) channel-id
                       :content (str "```clojure\n"
                                     (with-out-str
                                       (pprint/pprint (mafia.s/game-state state guild-id)))
                                     "```"))))

(defn invalid-mafia-command
  [{:keys [channel-id guild-id]}]
  (let [prefix (or (select-first [ATOM :state (keypath guild-id) :prefix] state)
                   "!")]
    (m/create-message! (:messaging @state) channel-id
                       :content (str "Invalid command, maybe try running `"
                                     prefix "mafia help` to see what you can do."))))

(defn process-state-commands
  [{:keys [channel-id guild-id content] :as event-data}]
  (let [prefix (or (select-first [ATOM :state (keypath guild-id) :prefix] state)
                   "!")]
    (case (:phase (mafia.s/game-state state guild-id))
      :join-game (command-fns event-data prefix content
                   (#"mafia\s+join" #'mafia-join)
                   (#"mafia\s+leave" #'mafia-leave)
                   (#"mafia" #'invalid-mafia-command))
      :night (command-fns event-data prefix content
               (#"mafia" #'invalid-mafia-command))
      :last-words (command-fns event-data prefix content
                    (#"mafia" #'invalid-mafia-command))
      :nominate (command-fns event-data prefix content
                  (#"mafia" #'invalid-mafia-command))
      :vote (command-fns event-data prefix content
              (#"mafia" #'invalid-mafia-command))
      :lynching (command-fns event-data prefix content
                  (#"mafia" #'invalid-mafia-command))
      nil (command-fns event-data prefix content
            (#"mafia" #'invalid-mafia-command)))))
