(ns clojure-lox.core
  (:require [failjure.core :as f]
            [clojure-lox.scanner :as scanner]
            [clojure-lox.parser :as parser]
            [clojure-lox.interpreter :as interpreter]
            [clojure-lox.error :as e]))

(defn run [source-string]
  (let [[tokens scanning-errors] (scanner/scan-tokens source-string)
        statements-or-error (parser/parse tokens)]
    (cond
      (and (seq scanning-errors)
           (f/failed? statements-or-error))
      (e/fail {:type   ::syntax-errors
               :errors (conj scanning-errors statements-or-error)})

      (and (seq scanning-errors)
           (not (f/failed? statements-or-error)))
      (e/fail {:type   ::syntax-errors
               :errors scanning-errors})

      (and (empty? scanning-errors)
           (f/failed? statements-or-error))
      (e/fail {:type   ::syntax-errors
               :errors [statements-or-error]})

      :else
      (interpreter/interpret statements-or-error))))
