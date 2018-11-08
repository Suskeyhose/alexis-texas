(ns alexis-texas.events)

(defonce state (atom nil))

(defmulti handle-event
  ""
  (fn [event-type event-data]
    event-type))
