(ns clojure-lox.environment
  (:refer-clojure :exclude [get])
  (:require [clojure-lox.error :as e]))

(defonce values (atom {}))

(defn clear! []
  (reset! values {}))

(defn define [name value]
  (swap! values assoc name value))

(defn get [name]
  (if-let [value (clojure.core/get @values name)]
    value
    (e/fail {:type ::undefined-variable})))
