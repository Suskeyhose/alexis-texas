(ns alexis-texas.events
  (:use
   com.rpl.specter)
  (:require
   [alexis-texas.util :as util]
   [clojure.string :as str]
   [taoensso.timbre :as log]
   [discljord.messaging :as m]))

(defonce state (atom nil))

(def ^:private roles-xf (map (fn [role]
                               [(:id role)
                                (dissoc role :id)])))
(def ^:private users-xf (map (fn [member]
                               [(:id (:user member))
                                (dissoc (:user member) :id)])))
(def ^:private members-xf (map (fn [member]
                                 [(:id (:user member))
                                  (dissoc member :user)])))

(defn ready
  [{{:keys [id username]} :user :as event-data}]
  (swap! state assoc :bot-id id
                     :bot-name username))

(defn- cleanse-guild
  [guild]
  (multi-transform
   (multi-path
    [:channels
     (view (partial group-by :id))
     MAP-VALS
     (terminal #(dissoc (first %) :id))]
    [:roles
     (view (partial group-by :id))
     MAP-VALS
     (terminal #(dissoc (first %) :id))]
    [:members
     (view (partial group-by (comp :id :user)))
     MAP-VALS
     (terminal #(dissoc (first %) :user))])
   guild))

(defn update-guild
  [{:keys [id] :as guild}]
  (multi-transform
   [ATOM
    (multi-path
     [:guilds (keypath id)
      (terminal
       (fn [state-guild]
         (merge
          state-guild
          (cleanse-guild guild))))]
     [:users
      (terminal
       (fn [users-map]
         (merge
          users-map
          (select-one [:members (subselect [ALL :user])
                       (view (partial group-by :id))
                       (transformed MAP-VALS first)
                       (view #(dissoc % :id))]
                      guild))))])]
   state))

(defn remove-guild
  [{:keys [id unavailable] :as event}]
  (when-not unavailable
    (setval [ATOM [:guilds (keypath id)]] NONE state)))

(defn update-channel
  [{:keys [id guild-id] :as channel}]
  (when-not guild-id
    (log/error "No guild id was provided when updating channel!" channel))
  (transform [ATOM :guilds (keypath guild-id) :channels (keypath id) (nil->val {})] #(merge % channel) state))

(defn remove-channel
  [{:keys [id guild-id] :as channel}]
  (when-not guild-id
    (log/error "No guild id was provided when removing channel!" channel))
  (setval [ATOM :guilds (keypath guild-id) :channels (keypath id)] NONE state))

(defn ban-blacklisted-member-on-join
  [{:keys [guild-id] {:keys [id bot username] :as user} :user :as event}]
  (when-not bot
    (when-let  [blacklisted (some #(if (instance? java.util.regex.Pattern %)
                                     (re-find % username)
                                     (when (str/includes? (str/lower-case username)
                                                          (str/lower-case %))
                                       %))
                                  (select [ATOM :guilds guild-id :blacklist ALL] state))]
      ;; if blacklisted, ban them
      (m/create-guild-ban! (:messaging @state) guild-id id
                           :reason (format
                                    (str "Alexis Texas auto-ban: had blacklisted pattern %s,"
                                         " in username \"%s\"")
                                    (prn-str blacklisted)
                                    username)))))

(defn add-member-info
  [{:keys [guild-id] {id :id :as user} :user :as event}]
  (let [member (dissoc event :user :guild-id)
        user (dissoc user :id)]
    (multi-transform [ATOM (multi-path [:guilds (keypath guild-id) :members (keypath id)
                                        (terminal-val member)]
                                       [:users (keypath id) (terminal-val user)])]
                     state)))

(defn add-guild-members
  [{:keys [guild-id members] :as event}]
  (multi-transform [ATOM (multi-path [:guilds (keypath guild-id) :members
                                      (terminal
                                       (fn [state-members]
                                         (merge state-members
                                                (transform [(view (partial group-by (comp :id :user)))
                                                            MAP-VALS]
                                                           #(dissoc (first %) :id)
                                                           members))))]
                                     [:users (terminal
                                              (fn [state-users]
                                                (merge-with
                                                 merge
                                                 state-users
                                                 (transform [ALL :user
                                                             (view (partial group-by :id))
                                                             MAP-VALS]
                                                            #(dissoc (first %) :id)
                                                            members))))])]
                   state))

(defn update-guild-member
  [{:keys [guild-id roles nick] {:keys [id]} :user :as event}]
  (multi-transform [ATOM :guilds (keypath guild-id) :members (keypath id)
                    (multi-path [:roles (terminal-val roles)]
                                [:nick (terminal-val nick)])]
                   state))

(defn remove-guild-member
  [{:keys [guild-id] {:keys [id]} :user}]
  (setval [ATOM :guilds (keypath guild-id) :members (keypath id)] NONE state))

(defn add-role
  [{:keys [guild-id role]}]
  (setval [ATOM :guilds (keypath guild-id) :roles (keypath (:id role))]
          (dissoc role :id)
          state))

(defn update-role
  [{:keys [guild-id role]}]
  (transform [ATOM :guilds (keypath guild-id) :roles (keypath (:id role))]
             #(merge % (dissoc role :id))
             state))

(defn delete-role
  [{:keys [guild-id role-id]}]
  (multi-transform [ATOM :guilds (keypath guild-id)
                    (multi-path [:roles (keypath role-id) (terminal-val NONE)]
                                [:members :roles (terminal (partial remove role-id))])]
                   state))

(defn update-user
  [{:keys [id] :as user}]
  (transform [ATOM :users (keypath id)] #(merge % (dissoc user :id)) state))

(defn disconnect-bot
  [event-data]
  (log/debug "Disconnecting from Discord.")
  (m/stop-connection! (:messaging @state))
  (swap! state assoc :running false)
  (util/save-state (:state @state)))
