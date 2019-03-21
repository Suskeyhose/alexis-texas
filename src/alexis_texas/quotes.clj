(ns alexis-texas.quotes
  (:use
   com.rpl.specter)
  (:require
   [alexis-texas.events :refer [state]]
   [discljord.messaging :as m]))

(defn add-quote
  [{:keys [channel-id guild-id]} user q]
  (m/create-message! (:messaging @state) channel-id
                     :content (str "Adding quote to user " user))
  (transform [ATOM :state (keypath guild-id) :quotes (keypath user)] #(conj (or % []) q) state))

(defn remove-quote
  [{:keys [channel-id guild-id]} user q]
  (m/create-message! (:messaging @state) channel-id
                     :content (str "Removing quote from user " user))
  (transform [ATOM :state (keypath guild-id) :quotes (keypath user)]
             #(filter (partial not= q) %)
             state))

(defn quote-for-user
  [{:keys [guild-id channel-id]} user]
  (let [quotes-vec (select [ATOM :state (keypath guild-id) :quotes (keypath user) ALL] state)]
    (if-not (empty? quotes-vec)
      (m/create-message! (:messaging @state) channel-id
                         :content (str user ": " (rand-nth quotes-vec)))
      (m/create-message! (:messaging @state) channel-id
                         :content (str "No quotes found for user " user "!")))))

(defn rand-quote
  [{:keys [guild-id channel-id]}]
  (let [quotes-vec (filter #(pos? (count (second %)))
                           (select [ATOM :state (keypath guild-id) :quotes ALL] state))]
    (if-not (empty? quotes-vec)
      (let [[user quotes] (rand-nth quotes-vec)]
        (m/create-message! (:messaging @state) channel-id
                           :content (str user ": " (rand-nth quotes))))
      (m/create-message! (:messaging @state) channel-id
                         :content "No quotes in this server! Get to talking!"))))
