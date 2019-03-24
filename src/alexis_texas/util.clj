(ns alexis-texas.util
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]))

(defn resource
  [path]
  (-> path
      io/resource
      slurp
      str/trim))

(def owner (resource "owner.txt"))

(defn save-state
  [state]
  (let [trimmed-state (into {}
                            (map (fn [[k v]]
                                   [k (dissoc v :mafia)])
                                 state))]
    (spit "quotes.edn" (pr-str trimmed-state))))
