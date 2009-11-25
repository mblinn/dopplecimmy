(ns com.randomfault.dopplecimmy
 (:import
   [org.jibble.pircbot PircBot])
 (:require
   [com.randomfault.markov :as brains]
   [clojure.contrib.seq-utils :as seq])
 (:gen-class
   :extends org.jibble.pircbot.PircBot
   :init init
   :post-init post-init))

(def nick "cimeleon")
(def json "/Users/mbevil0249k/Downloads/milhouse.json")

(def current (ref "mblinn"))

(defn -init []
  (brains/parse-json json))

(defn -post-init [this]
  (.setName this nick))

(defn permutations [tokens]
  (set (for [x tokens y tokens] (list x y))))

(defmulti respond
  (fn [tokens]
    (cond
      (.startsWith (first tokens) ",") :command
      true :message)))

(defmethod respond :message [tokens]
  (let [possible-firsts (permutations tokens)]
    (loop [possible-firsts possible-firsts]
      (let [first (seq/rand-elt (seq possible-firsts))
            sentence (brains/create-sentence first @current)]
        (cond
          (= 0 (count possible-firsts)) "I have no words."
          (>= (count (.split sentence " ")) 5) sentence
          true (recur (disj possible-firsts first)))))))

(defmethod respond :command [tokens]
  (cond
    (.equalsIgnoreCase (first tokens) ",become")
    (if (>= (count tokens) 2)
      (dosync (ref-set current (nth tokens 1)))
      "Who do you want me to be?")
    (.equalsIgnoreCase (first tokens) ",who")
    (format "I am %s" @current)))

(defn -onMessage [this channel sender login hostname message]
  (if (.startsWith message nick)
    (let [response (respond (rest (.split message " ")))]
      (.sendMessage this channel response))))
