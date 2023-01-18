(ns clojure-lox.error
  (:require [failjure.core :as failjure]))

(defrecord SimpleFailure []
  failjure/HasFailed
  (failed? [_] true)
  (message [_] nil))

(def fail "Constructs a failure out of any map."
  map->SimpleFailure)

(defn first-failure [coll]
  (reduce (fn [out v]
            (if (failjure/failed? v)
              (reduced v)
              (conj out v)))
          []
          coll))

(defn error
  [type line message]
  (fail {:type    type
         :line    line
         :message message}))
