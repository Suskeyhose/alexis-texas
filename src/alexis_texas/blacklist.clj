(ns alexis-texas.blacklist
  (:use
   com.rpl.specter)
  (:require
   [alexis-texas.events :refer [state]]
   [alexis-texas.permissions :refer [user-has-permission?]]
   [alexis-texas.util :refer [owner]]
   [discljord.messaging :as m]))

(defn add-blacklist-regex
  [{:keys [guild-id channel-id] {id :id} :author} blacklist-item]
  (let [admin? (or (= id owner)
                   (= id (select-any [ATOM :guilds (keypath guild-id) :owner-id] state))
                   (user-has-permission? id guild-id :manage-guild))]
    (when admin?
      (transform [ATOM :state (keypath guild-id) :blacklist]
                 #(conj (or % []) (re-pattern blacklist-item))
                 state)
      (m/create-message! (:messaging @state) channel-id
                         :content "Adding new blacklisted pattern"))))

(defn add-blacklist-string
  [{:keys [guild-id channel-id] {id :id} :author} blacklist-item]
  (let [admin? (or (= id owner)
                   (= id (select-any [ATOM :guilds (keypath guild-id) :owner-id] state))
                   (user-has-permission? id guild-id :manage-guild))]
    (when admin?
      (transform [ATOM :state (keypath guild-id) :blacklist]
                 #(conj (or % []) blacklist-item)
                 state)
      (m/create-message! (:messaging @state) channel-id
                         :content "Adding new blacklisted name"))))

(defn list-blacklist
  [{:keys [guild-id channel-id] {id :id} :author}]
  (let [admin? (or (= id owner)
                   (= id (select-any [ATOM :guilds (keypath guild-id) :owner-id] state))
                   (user-has-permission? id guild-id :manage-guild))]
    (when admin?
      (m/create-message! (:messaging @state) channel-id
                         :content (apply str "Blacklisted names:\n"
                                         (interpose
                                          "\n"
                                          (map-indexed
                                           #(str %1 ": " %2)
                                           (select [ATOM :state (keypath guild-id) :blacklist ALL]
                                                   state))))))))

(defn clear-blacklist
  [{:keys [guild-id channel-id] {id :id} :author}]
  (let [admin? (or (= id owner)
                   (= id (select-any [ATOM :guilds (keypath guild-id) :owner-id] state))
                   (user-has-permission? id guild-id :manage-guild))]
    (when admin?
      (setval [ATOM :state (keypath guild-id) :blacklist ALL] NONE state)
      (m/create-message! (:messaging @state) channel-id
                         :content "Clearing the blacklist!"))))

(defn remove-from-blacklist
  [{:keys [channel-id guild-id] {id :id} :author} idx]
  (let [admin? (or (= id owner)
                   (= id (select-any [ATOM :guilds (keypath guild-id) :owner-id] state))
                   (user-has-permission? id guild-id :manage-guild))]
    (when admin?
      (let [num (Long/parseLong idx)
            item (select-first [ATOM :state (keypath guild-id) :blacklist (keypath num)] state)]
        (setval [ATOM :state (keypath guild-id) :blacklist (keypath num)] NONE state)
        (m/create-message! (:messaging @state) channel-id
                           :content (str "Removing blacklist item: "
                                         item))))))
