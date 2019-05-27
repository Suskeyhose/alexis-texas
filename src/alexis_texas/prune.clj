(ns alexis-texas.prune
  (:use
   com.rpl.specter)
  (:require
   [alexis-texas.events]
   [alexis-texas.permissions :as a.p]
   [discljord.messaging :as d.m]
   [taoensso.timbre :as log]
   [clojure.set :as set])
  (:import
   (java.time
    Instant)
   (java.time.format
    DateTimeFormatter)
   (java.time.temporal
    ChronoUnit)))

(defn- bot?
  [user-id]
  (selected-any? [ATOM :users (keypath user-id) :bot (nil->val NONE)] alexis-texas.events/state))

(defn users-that-joined-after-instant
  [state guild-id after-instant]
  (select [:guilds (keypath guild-id) :members
           (transformed [MAP-VALS :joined-at]
                        #(Instant/from (.parse DateTimeFormatter/ISO_OFFSET_DATE_TIME %)))
           ALL
           (not-selected? FIRST bot?)
           (not-selected? LAST :joined-at
                          #(.isAfter %
                                     after-instant))
           FIRST]
          state))

(defn text-channel-ids
  [state guild-id]
  (select [:guilds (keypath guild-id) :channels ALL
           (selected? LAST (pred (comp zero? :type)))
           FIRST]
          state))

(defn viewable-text-channels
  [state guild-id]
  (let [user-id (:bot-id state)]
    (filter #(a.p/user-has-permission-for-channel? user-id guild-id % :view-channel)
            (text-channel-ids state guild-id))))

(defn prune-list
  [state channel-id guild-id duration]
  (log/debug "Starting a prune list!")
  (let [after-instant (.minus (Instant/now)
                              (long duration)
                              ChronoUnit/DAYS)
        users (users-that-joined-after-instant state guild-id after-instant)
        all-channels (text-channel-ids state guild-id)
        channels (viewable-text-channels state guild-id)
        invisible-channels (set/difference (set all-channels) (set channels))
        _ (when-not (empty? invisible-channels)
            (d.m/create-message! (:messaging state) channel-id
                                 :content (apply
                                           str
                                           "This prune may not be totally accurate, "
                                           "because I am unable to view some channels: "
                                           (map #(str "<#" % "> ") invisible-channels))))
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
