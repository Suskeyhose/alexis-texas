(ns alexis-texas.mafia.commands
  (:use
   com.rpl.specter)
  (:require
   [alexis-texas.events :refer [state]]
   [alexis-texas.mafia :as mafia]
   [alexis-texas.mafia.state :as mafia.s]
   [alexis-texas.permissions :refer [user-has-permission?]]
   [alexis-texas.util :refer [owner]]
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

(defn mafia-help
  [{:keys [guild-id channel-id] {id :id} :author}]
  ;; TODO: Make this state-dependant for the guild
  (let [admin? (or (= id owner)
                   (= id (select-any [ATOM :guilds (keypath guild-id) :owner-id] state))
                   (user-has-permission? id guild-id :manage-guild))
        prefix (or (select-first [ATOM :state (keypath guild-id) :prefix] state)
                   "!")]
    (m/create-message! (:messaging @state) channel-id
                       :content (mafia-help-message prefix admin?))))

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
  [{:keys [guild-id channel-id] {id :id} :author}]
  (m/create-message!
   (:messaging @state) channel-id
   :content (let [game-state (mafia.s/game-state state guild-id)]
              (if (:playing? game-state)
                (if (= (:phase game-state)
                       :join-game)
                  (if-not (contains? (:state game-state) id)
                    (do (mafia.s/set-game-state state guild-id
                                                (assoc game-state :state
                                                       (conj (or (:state game-state) #{}) id)))
                        (str "User " (:username ((:users @state) id)) " joined the mafia game!"))
                    (str "You're already in the game!"))
                  (str "The current game is past the join phase."
                       " Maybe you can play the next one!"))
                (str "There's no mafia game currently running on this server."
                     " Maybe you should start one!")))))

(defn mafia-leave
  [{:keys [guild-id channel-id] {id :id} :author}]
  (m/create-message!
   (:messaging @state) channel-id
   :content
   (let [game-state (mafia.s/game-state state guild-id)]
     (if (:playing? game-state)
       (if (= (:phase game-state)
              :join-game)
         (do (mafia.s/set-game-state state guild-id
                                     (assoc game-state :state
                                            (disj (:state game-state)
                                                  id)))
             (str "User " (:username ((:users @state) id)) " left the mafia game.")))
       (str "There's no game currently active! You can start one if you want to leave.")))))

(defn mafia-stop
  [{:keys [guild-id channel-id] {id :id} :author}]
  (let [admin? (or (= id owner)
                   (= id (select-any [ATOM :guilds (keypath guild-id) :owner-id] state))
                   (user-has-permission? id guild-id :manage-guild))]
    (when admin?
      (m/create-message!
       (:messaging @state) channel-id
       :content
       (if (:playing? (mafia.s/game-state state guild-id))
         (do (mafia.s/stop-game! state guild-id)
             (str "Stopping the mafia game!"))
         (str "No game is currently running."))))))

(defn mafia-phase-next
  [{:keys [guild-id channel-id] {id :id} :author}]
  (let [admin? (or (= id owner)
                   (= id (select-any [ATOM :guilds (keypath guild-id) :owner-id] state))
                   (user-has-permission? id guild-id :manage-guild))]
    (when (and admin?
               (:playing? (mafia.s/game-state state guild-id)))
      (mafia.s/advance-phase state guild-id)
      (m/create-message! (:messaging @state) channel-id
                         :content (str "Advancing game to next phase")))))

(defn invalid-mafia-command
  [{:keys [channel-id guild-id]}]
  (let [prefix (or (select-first [ATOM :state (keypath guild-id) :prefix] state)
                   "!")]
    (m/create-message! (:messaging @state) channel-id
                       :content (str "Invalid command, maybe try running `"
                                     prefix "mafia help` to see what you can do."))))
