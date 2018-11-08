(ns alexis-texas.core
  (:require
   [alexis-texas.commands]
   [alexis-texas.events :refer [handle-event state]]
   [alexis-texas.util :refer [resource]]
   [clojure.core.async :as a]
   [clojure.edn :as edn]
   [clojure.tools.logging :as log]
   [discljord.connections :as c]
   [discljord.events :as e]
   [discljord.messaging :as m])
  (:import
   (java.io FileNotFoundException))
  (:gen-class))


(def token (resource "token.txt"))

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
                   :state init-state
                   :roles {}
                   :users {}
                   :guilds {}
                   :running true})
    (a/go-loop []
      (a/<! (a/timeout 300000))
      (spit "quotes.edn" (pr-str (:state @state)))
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
