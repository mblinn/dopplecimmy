(ns com.randomfault.markov
 (:use 
   [clojure.contrib.test-is])
 (:require
   [clojure.set :as set]
   [clojure.contrib.seq-utils :as seq]
   [org.danlarkin.json :as json]))

(def nick-sets (ref {}))

(def sentence-max 20)

;; functions to create nick sets
(with-test

  (defn add-to-or-create-follow-set [first follow follow-sets]
    ; there's already a set of follow sets for the nick we're working with now.
    (if follow-sets
      (let [follow-set (follow-sets first)]
        (if follow-set
          (assoc follow-sets first (conj follow-set follow))
          (assoc follow-sets first #{follow})))
    ; there's no set of follow sets for the nick we're working with now, so
    ; we need to create it first
      (assoc {} first #{follow})))

  ;tests
  (is (= {'(:a :b) #{:c}} (add-to-or-create-follow-set '(:a :b) :c {})))
  (is (= {'(:a :b) #{:c :d}}
        (add-to-or-create-follow-set '(:a :b) :d {'(:a :b) #{:c}})))
  (is (= {'(:a :b) #{:c}} (add-to-or-create-follow-set '(:a :b) :c nil))))


(with-test

  (defn add-line-to-nick-set [line nick-set]
    (let [to-add (partition 3 1 (.split line " "))]
      (reduce (fn [nick-set current-addition]
                (let [first (take 2 current-addition) follow (last current-addition)]
                  (add-to-or-create-follow-set first follow nick-set)))
        nick-set
        to-add)))

  ;tests
  (is (= {'("a" "b") #{"c"}} (add-line-to-nick-set "a b c" {})))
  (is (= {'("a" "b") #{"c"}'("b" "c") #{"d"}}
        (add-line-to-nick-set "a b c d" {})))
  (is (= {'("a" "b") #{"c" "d"}}
        (add-line-to-nick-set "a b c" {'("a" "b") #{"d"}}))))


(defn parse-json [file-name]
  (doseq [element (json/decode-from-reader (java.io.FileReader. file-name))]
    (let [line (element 1) nick (element 0)]
      (dosync
        (ref-set
          nick-sets
          (assoc @nick-sets nick
            (add-line-to-nick-set line (@nick-sets nick))))))))


;; functions that create sentences
;
(with-test

  (defn rand-follow [first follow-set]
    (seq/rand-elt (seq (follow-set first))))

  ;tests
  (is (= "the" (rand-follow '("we" "are") {'("we" "are") #{"the"}})))
  (is (contains? #{"the" "it" "foo"}
        (rand-follow '("we" "are") {'("we" "are") #{"the" "it" "foo"}}))))

(defn create-sentence-vec [first-token follow-set]
  (loop [current-token first-token
         next-word (rand-follow first-token follow-set)
         sentence (vec (seq/flatten first-token))]
    (cond
      (nil? next-word) sentence
      (>= (count sentence) sentence-max) sentence
      true (let [next-token (list (second current-token) next-word)]
             (recur next-token (rand-follow next-token follow-set)
               (conj sentence next-word))))))

(defn create-sentence [first-token nick]
  (let [sentence-buffer (StringBuffer.)
        sentence-vec (create-sentence-vec first-token (@nick-sets nick))]
    (doseq [word sentence-vec]
      (doto sentence-buffer
        (.append word)
        (.append " ")))
    (.toString sentence-buffer)))

