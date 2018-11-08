(ns alexis-texas.permissions
  (:use
   com.rpl.specter)
  (:require
   [alexis-texas.events :refer [handle-event state]]
   [clojure.string :as str]
   [discljord.messaging :as m]))

(def permissions-bit {:create-instant-invite 0x1
                      :kick-members 0x2
                      :ban-members 0x4
                      :administrator 0x8
                      :manage-channels 0x10
                      :manage-guild 0x20
                      :add-reactions 0x40
                      :view-audit-log 0x80
                      :view-channel 0x400
                      :send-messages 0x800
                      :send-tts-messages 0x1000
                      :manage-messages 0x2000
                      :embed-links 0x4000
                      :attach-files 0x8000
                      :read-message-history 0x10000
                      :mention-everyone 0x20000
                      :use-external-emojis 0x40000
                      :connect 0x100000
                      :speak 0x200000
                      :mute-members 0x400000
                      :deafen-members 0x800000
                      :move-members 0x1000000
                      :use-vad 0x2000000
                      :priority-speaker 0x100
                      :change-nickname 0x4000000
                      :manage-nicknames 0x8000000
                      :manage-roles 0x10000000
                      :manage-webooks 0x20000000
                      :manage-emojis 0x40000000})

(defn has-permission?
  [perm perms-int]
  (when perms-int
    (when-let [bit (permissions-bit perm)]
      (not (zero? (bit-and bit perms-int))))))

(defn has-permissions?
  [perms perms-int]
  (every? #(has-permission? % perms-int) perms))

(defn user-has-permission?
  [user guild-id perm]
  (let [user-roles (conj (:roles (get (:guilds (get (:users @state) user)) guild-id))
                         guild-id)
        roles-permissions (map (fn [[_ {:keys [permissions]}]]
                                 permissions)
                               (select-keys (get (:roles @state) guild-id) user-roles))
        permissions-int (if (> (count roles-permissions) 1)
                          (apply bit-or roles-permissions)
                          (first roles-permissions))]
    (has-permission? perm permissions-int)))

(defn user-has-permissions?
  [user guild-id perms]
  (every? #(user-has-permission? user guild-id %) perms))

(def ^:private roles-xf (map (fn [role]
                               [(:id role)
                                (dissoc role :id)])))
(def ^:private users-xf (map (fn [member]
                               [(:id (:user member))
                                (dissoc (:user member) :id)])))
(def ^:private members-xf (map (fn [member]
                                 [(:id (:user member))
                                  (dissoc member :user)])))

(defn update-guild
  [{:keys [id roles members] :as guild}]
  (transform [ATOM :guilds (keypath id)]
             #(merge % (dissoc guild :id :roles :members))
             state)
  (let [roles (into {} roles-xf roles)
        users (into {} users-xf members)
        members (into {} members-xf members)]
    (doseq [[role-id role] roles]
      (setval [ATOM :roles (keypath id) (keypath role-id)] role state))
    (doseq [[user-id user] users]
      (when-not (select-any [ATOM :users (keypath user-id)] state)
        (setval [ATOM :users (keypath user-id)] user state)))
    (doseq [[user-id member] members]
      (setval [ATOM :users (keypath user-id) :guilds (keypath id)] member state))))

(defmethod handle-event :guild-create
  [_ guild]
  (update-guild guild))

(defmethod handle-event :guild-update
  [_ guild]
  (update-guild guild))

(defmethod handle-event :guild-remove
  [_ {:keys [id unavailable] :as event}]
  (when-not unavailable
    (setval [ATOM :guilds (keypath id)] NONE state)
    (setval [ATOM :users ALL :guilds (keypath id)] NONE state)
    (setval [ATOM :roles (keypath id)] NONE state)))

(defmethod handle-event :guild-member-add
  [_ {:keys [guild-id] {:keys [id bot username] :as user} :user :as event}]
  (when-not bot
    (if-let  [blacklisted (some #(if (instance? java.util.regex.Pattern %)
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
                                    username))
      ;; otherwise, add them to the members
      (let [member (dissoc event :user :guild-id)]
        (when-not (select-any [ATOM :users (keypath id)] state)
          (setval [ATOM :users (keypath id)] (dissoc user :id) state))
        (setval [ATOM :users (keypath id) :guilds (keypath guild-id)] member state)))))

(defmethod handle-event :guild-members-chunk
  [_ {:keys [guild-id members] :as event}]
  (let [members (into {}
                      members-xf
                      members)]
    (doseq [[user-id member] members]
      (setval [ATOM :users (keypath user-id) :guilds (keypath guild-id)] member state))))

(defmethod handle-event :guild-member-update
  [_ {:keys [guild-id roles nick]
      {:keys [id]} :user
      :as event}]
  (setval [ATOM :users (keypath id) :guilds (keypath guild-id) :roles] roles state)
  (setval [ATOM :users (keypath id) :guilds (keypath guild-id) :nick] nick state))

(defmethod handle-event :guild-member-remove
  [_ {:keys [guild-id]
      {:keys [id]} :user}]
  (setval [ATOM :users (keypath id) :guilds (keypath guild-id)] NONE state))

(defmethod handle-event :guild-role-create
  [_ {:keys [guild-id role]}]
  (setval [ATOM :roles (keypath guild-id) (keypath (:id role))] (dissoc role :id) state))

(defmethod handle-event :guild-role-update
  [_ {:keys [guild-id role]}]
  (transform [ATOM :roles (keypath guild-id) (keypath (:id role))]
             #(merge % (dissoc role :id))
             state))

(defmethod handle-event :guild-role-delete
  [_ {:keys [guild-id role-id]}]
  (setval [ATOM :roles (keypath guild-id) (keypath role-id)] NONE state)
  (transform [ATOM :users ALL :guilds (keypath guild-id) :roles]
             (partial remove #(= % role-id))
             state))

(defmethod handle-event :user-update
  [_ {:keys [id] :as user}]
  (transform [ATOM :users (keypath id)] #(merge % (dissoc user :id)) state))
