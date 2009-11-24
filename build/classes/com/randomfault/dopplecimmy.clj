(ns com.randomfault.dopplecimmy
 (:import
   [org.jibble.pircbot PircBot])
 (:require
   [com.randomfault.markov :as brains])
 (:gen-class
   :extends org.jibble.pircbot.PircBot
   :init init
   :post-init post-init))

(def nick "cimeleon")
(def json "/Users/mbevil0249k/Downloads/milhouse.json")

(defn -init []
  (brains/parse-json json))

(defn -post-init [this]
  (.setName this nick))

(defn create-message [line nick]
  (let [tokens (.split line " ")
        first (list (nth tokens 1) (nth tokens 2))]
    (do
      (println (brains/create-sentence first "mblinn"))
      (brains/create-sentence first "mblinn"))))

(defn -onMessage [this channel sender login hostname message]
  (if (.startsWith message nick)
    (.sendMessage this "#milhouse" (create-message message "mblinn"))))
