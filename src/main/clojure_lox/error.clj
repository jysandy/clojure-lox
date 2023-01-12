(ns clojure-lox.error)

(defn error
  [line message]
  {:line    line
   :message message})
