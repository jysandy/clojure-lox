(ns clojure-lox.statement
  (:require [clojure.spec.alpha :as s]
            [clojure-lox.expression :as expr]
            [clojure-lox.scanner :as scanner]
            [medley.core :as medley]))

(def statement-types (atom #{}))

(defn type-is [type]
  (fn [expr]
    (= type (:type expr))))

(s/def ::any (constantly true))
(s/def ::stmt (s/keys :req-un [::type]))

(defmacro stmt-spec [spec-name field-map]
  (let [prefixed-field-map (medley/map-keys (fn [k]
                                              (keyword (str (when (namespace spec-name)
                                                              (str (namespace spec-name) "."))
                                                            (name spec-name)
                                                            "/"
                                                            (name k))))
                                            field-map)
        keys-list          (keys prefixed-field-map)]
    `(do
       (swap! statement-types conj ~spec-name)
       (s/def ~spec-name (s/and (type-is ~spec-name)
                                (s/merge ::stmt (s/keys :req-un ~keys-list))))
       ~@(map (fn [[k v]]
                (when v
                  `(s/def ~k ~v)))
              prefixed-field-map))))

;; TODO: Pull out the common parts of the statement and expression namespaces
;; into an ast namespace

;; ------- Statements --------

(stmt-spec ::expression {:expression ::expr/expr})
(stmt-spec ::print {:expression ::expr/expr})
(stmt-spec ::var {:name        ::scanner/token
                  :initializer ::expr/expr})

;; ------- Valid statement types --------

(s/def ::type @statement-types)
