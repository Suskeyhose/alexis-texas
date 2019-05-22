(ns alexis-texas.prune
  (:use
   com.rpl.specter)
  (:require
   [discljord.messaging :as d.m]
   [taoensso.timbre :as log])
  (:import
   (java.time
    Instant)
   (java.time.format
    DateTimeFormatter)
   (java.time.temporal
    ChronoUnit)))

(defn prune-list
  [state guild-id duration]
  (log/debug "Starting a prune list!")
  (let [after-instant (.minus (Instant/now)
                              (long duration)
                              ChronoUnit/DAYS)
        users (sequence (comp (filter #((:guilds (second %)) guild-id))
                              (filter #(not= (first %) (:bot-id state)))
                              (remove #(:bot (second %)))
                              (remove #(.isAfter ^Instant
                                                 (Instant/from
                                                  (.parse DateTimeFormatter/ISO_OFFSET_DATE_TIME
                                                          (select-one [LAST :guilds
                                                                       (keypath guild-id) :joined-at]
                                                                      %)))
                                                 after-instant))
                              (map first))
                        (:users state))
        channels (select [:guilds (keypath guild-id) :channels ALL
                          (if-path (comp zero? :type)
                            STAY
                            STOP)
                          :id]
                         state)
        new-messages (mapcat
                      (fn [channel-id]
                        (log/debug "Getting messages for channel " channel-id)
                        (loop [most-recent-message nil
                               acc []]
                          (log/debug "Most recent message: " most-recent-message)
                          (log/debug "Number of messages so far: " (count acc))
                          (let [msgs (transform
                                      [ALL :timestamp]
                                      #(Instant/from (.parse DateTimeFormatter/ISO_OFFSET_DATE_TIME %))
                                      (if most-recent-message
                                        @(d.m/get-channel-messages! (:messaging state) channel-id
                                                                    :before most-recent-message
                                                                    :limit 100)
                                        @(d.m/get-channel-messages! (:messaging state) channel-id
                                                                    :limit 100)))
                                last-msg (when (pos? (count msgs))
                                           (nth msgs (dec (count msgs))))]
                            (if (or (< (count msgs) 100)
                                    (.isBefore ^Instant (:timestamp last-msg)
                                               after-instant))
                              (into acc (take-while #(.isBefore ^Instant (:timestamp %)
                                                                after-instant)
                                                    msgs))
                              (recur (:id last-msg)
                                     (into acc msgs))))))
                      channels)]
    (remove
     (fn [user]
       (first (filter #(= (:id (:author %)) user)
                      new-messages)))
     users)))
