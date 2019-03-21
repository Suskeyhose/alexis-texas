(ns alexis-texas.mafia.state
  (:use
   com.rpl.specter)
  (:require
   [alexis-texas.mafia :as m]
   [clojure.spec.alpha :as s]))

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
        players (players-map (:state s))]
    (transform [ATOM :state (keypath guild-id) :mafia]
               (fn [s]
                 (assoc s
                        :players players
                        :state []
                        :day 1
                        :phase :night))
               state)))

(defmethod advance-phase :night
  [state guild-id])

(defmethod advance-phase :nominate
  [state guild-id])

(defmethod advance-phase :vote
  [state guild-id])
