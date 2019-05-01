(ns alexis-texas.state
  (:use
   com.rpl.specter))

(defn get-prefix
  [state guild-id]
  (or (select-first [ATOM :state (keypath guild-id) :prefix] state)
      "!"))
