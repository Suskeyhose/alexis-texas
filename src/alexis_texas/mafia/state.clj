(ns alexis-texas.mafia.state
  (:use
   com.rpl.specter)
  (:require
   [alexis-texas.mafia :as m]
   [clojure.spec.alpha :as s]
   [discljord.messaging :as msg]))

(comment
  ;; Game state can be in any of several phases, :join-game, :night, :nominate, and :vote
  ;; State for :join-game is a vector of player IDs
  #{"1234567890" "0987654321"}
  ;; State for :night is a vector of events which will be accumulated
  {"1234567890" {:type :mafia/kill
                 :mafia "1234567890"
                 :target "0987654321"}}
  ;; State for :nominate is a set of nominated players
  #{"1234567890"}
  ;; State for :vote is a map of voters to who they vote for, and a set of possible players
  {:nominated #{"1234567890"}
   :votes {"0987654321" "1234567890"}})

(defn game-state
  [state guild-id]
  (select-first [ATOM :state (keypath guild-id) :mafia] state))

(defn update-game-state
  [state guild-id f]
  (transform [ATOM :state (keypath guild-id) :mafia] f state))

(defn set-game-state
  [state guild-id game-state]
  (setval [ATOM :state (keypath guild-id) :mafia] game-state state))

(defn start-game!
  [state guild-id channel-id]
  (setval [ATOM :state (keypath guild-id) :mafia]
          {:playing? true
           :phase :join-game
           :day 0
           :players {}
           :state #{}
           :channel channel-id}
          state))

(defn stop-game!
  [state guild-id]
  (setval [ATOM :state (keypath guild-id) :mafia :playing?] false state))

(defmulti advance-phase
  (fn [state guild-id]
    (:phase (game-state state guild-id))))

(defn map-counts
  [m]
  (into []
        (mapcat #(repeat (second %) (first %)))
        m))

(defn players-map
  [players]
  (let [player-roles (shuffle (map-counts (m/player-distrobution (count players))))]
    (reduce-kv (fn [m player-id player-role]
                 (assoc m player-id {:alive? true
                                     :role player-role}))
               {}
               (into {} (map vector players player-roles)))))

(defmethod advance-phase :join-game
  [state guild-id]
  (let [s (game-state state guild-id)
        players (players-map (:state s))
        channel-id (:channel s)]
    (update-game-state state guild-id
                       (fn [s]
                         (assoc s
                                :players players
                                :state []
                                :day 1
                                :phase :night)))
    (msg/create-message! (:messaging @state) channel-id
                         :content (str "The sun dips below the horizon, and the town goes to sleep.\n"
                                       "Tonight will be a rough night, you all can feel it...\n\n"))
    (when (= (:day s) 1)
      ;; TODO(Joshua): Tell the players how the night will play out, and what's going on
      )
    (msg/create-message! (:messaging @state) channel-id
                         :content (str "And so begins night " (:day s)))))

(defmethod advance-phase :night
  [state guild-id]
  (update-game-state state guild-id
                     (fn [s]
                       (assoc s
                              :phase :last-words)))
  ;; TODO(Joshua): Notify the players that they can no longer use their abilities, and that some
  ;;               players have been asked for their last words
  )

(defmethod advance-phase :last-words
  [state guild-id]
  (update-game-state state guild-id
                     (fn [s]
                       (assoc s
                              :phase :nominate)))
  ;; TODO(Joshua): Notify the players that it's time to nominate who they think is mafia, and if
  ;;               it's day 1, send a help message to show how to nominate someone
  )

(defmethod advance-phase :nominate
  [state guild-id]
  (update-game-state state guild-id
                     (fn [s]
                       (assoc s
                              :phase :vote)))
  ;; TODO(Joshua): Notify the players who has been nominated, and declare a vote. If it's the
  ;;               first day, display a help message, showing how to vote
  )

(defmethod advance-phase :vote
  [state guild-id]
  (update-game-state state guild-id
                     (fn [s]
                       (assoc s
                              :phase :lynching)))
  ;; TODO(Joshua): Notify the players who was voted out, and allow them a brief moment to say
  ;;               their last words
  )

(defmethod advance-phase :lynching
  [state guild-id]
  (update-game-state state guild-id
                     (fn [s]
                       (assoc s
                              :day (inc (:day s))
                              :phase :night)))
  ;; TODO(Joshua): Notify the players that it is now night, and send each of the players a dm
  ;;               telling them how they should act.
  )
