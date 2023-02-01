(ns clojure-lox.core-test
  (:require [clojure.test :refer :all]
            [clojure-lox.core :as lox]
            [clojure-lox.environment :as environment]))

(deftest run-test
  (is (= "11.0\n"
         (with-out-str
           (environment/clear!)
           (lox/run "var a = 5; var b = 6; print a + b;")))))