(ns clojure-lox.expression
  (:require [clojure.spec.alpha :as s]
            [clojure-lox.scanner :as scanner]
            [medley.core :as medley]))

(def expression-types (atom #{}))

(defn type-is [type]
  (fn [expr]
    (= type (:type expr))))

(s/def ::any (constantly true))
(s/def ::expr (s/keys :req-un [::type]))

(defmacro expr-spec [spec-name field-map]
  (let [prefixed-field-map (medley/map-keys (fn [k]
                                              (keyword (str (when (namespace spec-name)
                                                              (str (namespace spec-name) "."))
                                                            (name spec-name)
                                                            "/"
                                                            (name k))))
                                            field-map)
        keys-list          (keys prefixed-field-map)]
    `(do
       (swap! expression-types conj ~spec-name)
       (s/def ~spec-name (s/and (type-is ~spec-name)
                                (s/merge ::expr (s/keys :req-un ~keys-list))))
       ~@(map (fn [[k v]]
                (when v
                  `(s/def ~k ~v)))
              prefixed-field-map))))

;; ------- Expressions --------

(expr-spec ::binary {:left     ::expr
                     :right    ::expr
                     :operator ::scanner/token})
(expr-spec ::grouping {:expression ::expr})
(expr-spec ::literal {:value ::any})
(expr-spec ::unary {:operator ::scanner/token
                    :right    ::expr})

;; ------- Valid expression types --------

(s/def ::type @expression-types)