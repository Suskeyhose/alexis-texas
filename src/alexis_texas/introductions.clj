(ns alexis-texas.introductions
  (:use
   com.rpl.specter)
  (:require
   [alexis-texas.events :refer [state]]
   [alexis-texas.permissions :as a.p]
   [discljord.messaging :as d.m])
  (:import
   (java.util.regex
    Pattern)))

(defn set-channel
  "Sets the channel for introductions to be validated in"
  [{:keys [guild-id channel-id] {user-id :id} :author} intro-channel-id]
  (when (a.p/admin? guild-id user-id)
    (setval [ATOM :state (keypath guild-id) ::channel]
            intro-channel-id
            state)
    (d.m/create-message! (:messaging @state) channel-id
                         :content "Introductions channel set!")))

(defn get-channel
  "Gets the channel for introductions to be validated in"
  [{:keys [guild-id channel-id] {user-id :id} :author}]
  (when (a.p/admin? guild-id user-id)
    (let [channel (select-any [ATOM :state (keypath guild-id) ::channel] state)]
      (if channel
        (d.m/create-message! (:messaging @state) channel-id
                             :content (str "Introductions channel: <#" channel ">"))
        (d.m/create-message! (:messaging @state) channel-id
                             :content "No introductions channel set.")))))

(defn set-member-role
  "Sets the role to be given to a user on a valid intro"
  [{:keys [guild-id channel-id] {user-id :id} :author} role-id]
  (when (a.p/admin? guild-id user-id)
    (setval [ATOM :state (keypath guild-id) ::member-role]
            role-id
            state)
    (d.m/create-message! (:messaging @state) channel-id
                         :content "Member role set!")))

(defn get-member-role
  "Gets the role to be given to a user on a valid intro"
  [{:keys [guild-id channel-id] {user-id :id} :author}]
  (when (a.p/admin? guild-id user-id)
    (let [role (select-any [ATOM :state (keypath guild-id) ::member-role] state)]
      (if role
        (d.m/create-message! (:messaging @state) channel-id
                             :content (str "Member role: <@&" role ">"))
        (d.m/create-message! (:messaging @state) channel-id
                             :content "No member role set.")))))

(defn set-guest-role
  "Sets the role to be taken from a user on a valid intro"
  [{:keys [guild-id channel-id] {user-id :id} :author} role-id]
  (when (a.p/admin? guild-id user-id)
    (setval [ATOM :state (keypath guild-id) ::guest-role]
            role-id
            state)
    (d.m/create-message! (:messaging @state) channel-id
                         :content "Guest role set!")))

(defn get-guest-role
  "Gets the role to be given to a user on a valid intro"
  [{:keys [guild-id channel-id] {user-id :id} :author}]
  (when (a.p/admin? guild-id user-id)
    (let [role (select-any [ATOM :state (keypath guild-id) ::guest-role] state)]
      (if role
        (d.m/create-message! (:messaging @state) channel-id
                             :content (str "Guest role: <@&" role ">"))
        (d.m/create-message! (:messaging @state) channel-id
                             :content "No guest role set.")))))

(defn set-minimum-length
  "Sets the minimum length of a valid introduction"
  [{:keys [guild-id channel-id] {user-id :id} :author} length]
  (when (a.p/admin? guild-id user-id)
    (setval [ATOM :state (keypath guild-id) ::minimum-length]
            (Integer/parseInt length)
            state)
    (d.m/create-message! (:messaging @state) channel-id
                         :content "Minimum length set!")))

(defn get-minimum-length
  "Gets the minimum length of a valid introduction"
  [{:keys [guild-id channel-id] {user-id :id} :author}]
  (when (a.p/admin? guild-id user-id)
    (let [length (select-any [ATOM :state (keypath guild-id) ::minimum-length (nil->val 0)] state)]
      (d.m/create-message! (:messaging @state) channel-id
                           :content (str "Minimum length of an intro is "
                                         length
                                         " characters")))))

(defn add-regex-pattern
  "Adds a regex pattern to match a valid introduction"
  [{:keys [guild-id channel-id] {user-id :id} :author} pattern]
  (when (a.p/admin? guild-id user-id)
    (transform [ATOM :state (keypath guild-id) ::patterns NIL->VECTOR]
               #(conj % pattern)
               state)
    (d.m/create-message! (:messaging @state) channel-id
                         :content "Adding a regex pattern.")))

(defn add-string-pattern
  "Adds a string pattern to match a valid introduction"
  [{:keys [guild-id channel-id] {user-id :id} :author} pattern]
  (when (a.p/admin? guild-id user-id)
    (transform [ATOM :state (keypath guild-id) ::patterns NIL->VECTOR]
               #(conj % (Pattern/quote pattern))
               state)
    (d.m/create-message! (:messaging @state) channel-id
                         :content "Adding a string pattern.")))

(defn list-patterns
  "Lists all the patterns which are currently used"
  [{:keys [channel-id guild-id] {user-id :id} :author}]
  (when (a.p/admin? guild-id user-id)
    (let [patterns (select-any [ATOM :state (keypath guild-id) ::patterns NIL->VECTOR] state)
          lines (if (= 0 (count patterns))
                  '("No patterns currently required!")
                  (for [idx (range (count patterns))]
                    (let [pattern (str (nth patterns idx))
                          pattern (if (and (.startsWith pattern "\\Q")
                                           (.endsWith pattern "\\E"))
                                    (str " (**String**): `" (subs pattern 2 (- (count pattern) 2)))
                                    (str " (**Regex**): `" pattern))]
                      (str idx pattern "`\n"))))
          messages (reduce (fn [acc line]
                             (if (> (+ (count line)
                                       (count (nth acc (dec (count acc)))))
                                    2000)
                               (conj acc line)
                               (conj (subvec acc 0 (dec (count acc)))
                                     (str (nth acc (dec (count acc)))
                                          line))))
                           [(first lines)]
                           (rest lines))]
      (mapv #(d.m/create-message! (:messaging @state) channel-id
                                  :content %)
            messages))))

(defn delete-pattern
  "Removes a pattern at a given index"
  [{:keys [channel-id guild-id] {user-id :id} :author} idx]
  (when (a.p/admin? guild-id user-id)
    (let [idx (Integer/parseInt idx)]
      (transform [ATOM :state (keypath guild-id) ::patterns NIL->VECTOR]
                 #(if (and (nat-int? idx)
                           (< idx (count %)))
                    (do (d.m/create-message! (:messaging @state) channel-id
                                             :content (str "Removing pattern `" % "`"))
                        (vec (concat (take idx %)
                                     (drop (inc idx) %))))
                    (do (d.m/create-message! (:messaging @state) channel-id
                                             :content "No pattern with that index!")
                      %))
                 state))))

(defn reset-state
  "Removes all the keys from the state map"
  [{:keys [guild-id] {user-id :id} :author}]
  (when (a.p/admin? guild-id user-id)
    (transform [ATOM :state (keypath guild-id)]
               #(dissoc %
                        ::channel
                        ::member-role
                        ::guest-role
                        ::minimum-length
                        ::patterns)
               state)))

(defn intro-channel
  "Retrieves the channel id of the introductions channel"
  [guild-id]
  (select-any [ATOM :state (keypath guild-id) ::channel] state))

(defn valid-intro?
  "Returns if a given string is considered a valid intro for the given guild"
  [guild-id message]
  (let [state @state
        min-length (select-any [:state (keypath guild-id) ::minimum-length (nil->val 0)] state)
        patterns (select-any [:state (keypath guild-id) ::patterns NIL->VECTOR] state)]
    (boolean (and (> (count message) min-length)
                  (some #(re-find (Pattern/compile %) message) patterns)))))

(defn give-roles
  "Gives the member role and removes the guest role to the given user"
  [guild-id user-id]
  (let [guest (select-any [ATOM :state (keypath guild-id) ::guest-role] state)
        member (select-any [ATOM :state (keypath guild-id) ::member-role] state)]
    (when member
      (d.m/add-guild-member-role! (:messaging @state) guild-id user-id member))
    (when guest
      (d.m/remove-guild-member-role! (:messaging @state) guild-id user-id guest))))

(defn help-message
  "Sends a message which will be displayed when asking for help with introduction settings"
  [prefix]
  (str
   "I can be set up to detect introductions in a given channel and give users "
   "roles based on those introductions. The introduction has to meet a minimum "
   "length requirement and have at least one of a list of patterns match it. "
   "If a user makes a valid introduction in the introductions channel, they will "
   "be given the member role and have the guest role removed from them.\n"
   "\n"
   "`" prefix "introduction channel [channel id or mention]` sets the channel for introductions\n"
   "`" prefix "introduction channel` gets the channel for introductions\n"
   "`" prefix "introduction member [member role id or mention]` sets the member role\n"
   "`" prefix "introduction member` gets the member role\n"
   "`" prefix "introduction guest [guest role id or mention]` sets the guest role\n"
   "`" prefix "introduction guest` gets the guest role\n"
   "`" prefix "introduction length [number]` sets the minimum length of an introduction in characters\n"
   "`" prefix "introduction length` gets the minimum length of an introduction\n"
   "`" prefix "introduction pattern [regex]` adds a regex pattern as a valid intro\n"
   "`" prefix "introduction text [string]` adds a string pattern as a valid intro\n"
   "`" prefix "introduction patterns` lists all the current patterns by their indices\n"
   "`" prefix "introduction delete [pattern index]` deletes the pattern at the given index\n"
   "`" prefix "introduction reset` resets all the settings to their defaults "
   "(no channel, no roles, 0 length minimum)\n"
   "`" prefix "introduction help` show this help message"))
