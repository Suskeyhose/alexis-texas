(ns alexis-texas.core
  (:require
   [alexis-texas.commands :as commands]
   [alexis-texas.events :as events]
   [alexis-texas.util :as util]
   [clojure.core.async :as a]
   [clojure.edn :as edn]
   [taoensso.timbre :as log]
   [discljord.connections :as c]
   [discljord.events :as e]
   [discljord.messaging :as m])
  (:import
   (java.io FileNotFoundException))
  (:gen-class))


(def token (util/resource "token.txt"))

(defonce ^:dynamic *events* (atom nil))
(defonce ^:dynamic *connection* (atom nil))
(defonce ^:dynamic *messaging* (atom nil))

(defn handler
  [& {:as handlers}]
  (fn [event-type event-data]
    (doseq [f (event-type handlers)]
      (f event-data))))

(def handle-event
  (handler
   :ready [#'events/ready]
   :guild-create [#'events/update-guild]
   :guild-update [#'events/update-guild]
   :guild-remove [#'events/remove-guild]
   :guild-member-add [#'events/add-member-info]
   :guild-members-chunk [#'events/add-guild-members]
   :guild-member-update [#'events/update-guild-member]
   :guild-member-remove [#'events/remove-guild-member]
   :guild-role-create [#'events/add-role]
   :guild-role-update [#'events/update-role]
   :guild-role-delete [#'events/delete-role]
   :user-update [#'events/update-user]
   :message-create [#'commands/process-message]
   :disconnect [#'events/disconnect-bot]))

(defn run-bot
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
                   :state init-state
                   :roles {}
                   :users {}
                   :guilds {}
                   :running true})
    (a/go-loop []
      (a/<! (a/timeout 300000))
      (util/save-state (:state @state))
      (when (:running @state)
        (recur)))
    (e/message-pump! events #'handle-event)))

(defn -main
  "Starts the alexis-texas bot."
  []
  (run-bot)
  (shutdown-agents))

(defn start-bot!
  []
  (a/thread (run-bot)))

(defn stop-bot!
  []
  (when @*connection*
    (c/disconnect-bot! @*connection*))
  (when @*messaging*
    (m/stop-connection! @*messaging*))
  (reset! *events* nil)
  (reset! *connection* nil)
  (reset! *messaging* nil))
