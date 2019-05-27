(ns alexis-texas.state
  (:use
   com.rpl.specter))

(defn get-prefix
  [state guild-id]
  (select-one [ATOM :state (keypath guild-id) :prefix (nil->val "!")] state))
