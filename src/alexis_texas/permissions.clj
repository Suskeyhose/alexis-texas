(ns alexis-texas.permissions
  (:use
   com.rpl.specter)
  (:require
   [alexis-texas.events :refer [state]]
   [alexis-texas.util :refer [owner]]))

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

(defn reducing-bit-or
  ([] 0)
  ([v] v)
  ([x y] (bit-or x y)))

(defn user-has-permission?
  [user-id guild-id perm]
  (let [state @state
        user-roles (select [:guilds (keypath guild-id) :members (keypath user-id)
                            :roles (view #(conj % guild-id)) ALL]
                           state)
        permissions-int (select-one (traversed [:guilds (keypath guild-id)
                                                :roles (submap user-roles)
                                                MAP-VALS :permissions]
                                               reducing-bit-or)
                                    state)]
    (has-permission? perm permissions-int)))

(defn user-has-permissions?
  [user-id guild-id perms]
  (every? #(user-has-permission? user-id guild-id %) perms))

(defn role-has-permission?
  [role-id guild-id perm]
  (let [role-permissions (select [ATOM :guilds (keypath guild-id)
                                  :roles (keypath role-id) :permissions]
                                 state)]
    (has-permission? perm role-permissions)))

(defn role-has-permissions?
  [role-id guild-id perms]
  (every? #(role-has-permission? role-id guild-id %) perms))

(defn role-has-permission-for-channel?
  [role-id guild-id channel-id perm]
  (let [state @state
        role-permissions (select-one [:guilds (keypath guild-id) :roles (keypath role-id) :permissions] state)
        [deny allow] (select [:guilds (keypath guild-id) :channels
                              (keypath channel-id) :permission-overwrites
                              (filterer [:id (pred= role-id)])
                              FIRST (multi-path :deny :allow)]
                             state)]
    (bit-or (bit-and role-permissions
                     (bit-not deny))
            allow)))

(defn role-has-permissions-for-channel?
  [role-id guild-id channel-id perms]
  (every? #(role-has-permission-for-channel? role-id guild-id channel-id %) perms))

(defn user-has-permission-for-channel?
  [user-id guild-id channel-id perm]
  (let [state @state
        user-roles (set (select [:guilds (keypath guild-id) :members (keypath user-id) :roles ALL] state))
        [[everyone-deny everyone-allow]
         [roles-deny roles-allow]
         [user-deny user-allow]]
        (select [:guilds (keypath guild-id) :channels
                 (keypath channel-id) :permission-overwrites
                 (multi-path (filterer [:id (pred= guild-id)])
                             (filterer [(selected? [:type (pred= "role")])
                                        :id (pred user-roles)])
                             (filterer [(selected? [:type (pred= "member")])
                                        :id (pred= user-id)]))
                 (view (fn [[{:keys [deny allow]}]]
                         [(or deny 0) (or allow 0)]))]
                state)
        base-perms (select-one (traversed [:guilds (keypath guild-id) :roles
                                           (submap (conj user-roles guild-id))
                                           MAP-VALS :permissions]
                                          reducing-bit-or)
                               state)
        final-perms (bit-or
                     (bit-and
                      (bit-or
                       (bit-and
                        (bit-or
                         (bit-and base-perms
                                  (bit-not everyone-deny))
                         everyone-allow)
                        (bit-not roles-deny))
                       roles-allow)
                      (bit-not user-deny))
                     user-allow)]
    (has-permission? perm final-perms)))

(defn user-has-permissions-for-channel?
  [user-id guild-id channel-id perms]
  (every? #(user-has-permission-for-channel? user-id guild-id channel-id %) perms))

(defn admin?
  [guild-id user]
  (let [id (if (map? user)
             (:id user)
             user)]
    (or (= id owner)
        (= id (select-any [ATOM :guilds (keypath guild-id) :owner-id] state))
        (user-has-permission? id guild-id :manage-guild))))
