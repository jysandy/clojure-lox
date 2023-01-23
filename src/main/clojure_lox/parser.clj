(ns clojure-lox.parser
  (:require [clojure-lox.expression :as expression]
            [clojure-lox.error :as e]
            [failjure.core :as f]))

(def error (partial e/error ::error))

(defn token-error
  [token message]
  (if (= :eof (:token-type token))
    (error (:line token) (str "at end: " message))
    (error (:line token) (str "at '" (:lexeme token) "': " message))))

(declare expression)

(defn- token-matches? [token-types token]
  (contains? (set token-types) (:token-type token)))

(defn- consume [tokens token-index token-type]
  (if (= token-type (:token-type (get tokens token-index)))
    (inc token-index)
    (token-error (get tokens token-index) "Expect ')' after expression.")))

(defn- primary [tokens token-index]
  (condp token-matches? (get tokens token-index)
    #{:false} [{:type ::expression/literal :value false} (inc token-index)]
    #{:true} [{:type ::expression/literal :value true} (inc token-index)]
    #{:nil} [{:type ::expression/literal :value nil} (inc token-index)]
    #{:number :string} [{:type ::expression/literal :value (:literal (get tokens token-index))}
                        (inc token-index)]
    #{:left-paren} (f/attempt-all [expr-and-new-index (expression tokens (inc token-index))
                                   expr               (first expr-and-new-index)
                                   new-index          (consume tokens (second expr-and-new-index) :right-paren)]
                     [{:type       ::expression/grouping
                       :expression expr}
                      new-index])
    (token-error (get tokens token-index) "Expect expression")))

(defn- unary [tokens token-index]
  (if (token-matches? #{:bang :minus}
                      (get tokens token-index))
    (f/when-let-ok? [right-expr-and-new-index (unary tokens (inc token-index))]
      [{:type     ::expression/unary
        :operator (get tokens token-index)
        :right    (first right-expr-and-new-index)}
       (second right-expr-and-new-index)])
    (primary tokens token-index)))

(defn- binary-matcher [operator-token-types operand-fn]
  (fn [tokens token-index]
    ;; TODO: Destructure these after https://github.com/adambard/failjure/issues/31 is fixed
    (f/attempt-all [expr-and-new-index (operand-fn tokens token-index)]
      (loop [e (first expr-and-new-index)
             i (second expr-and-new-index)]
        (if (token-matches? operator-token-types
                            (get tokens i))
          (f/attempt-all [right-expr-and-new-index (operand-fn tokens (inc i))
                          [right-expr new-index] right-expr-and-new-index]
            (recur {:type     ::expression/binary
                    :left     e
                    :right    right-expr
                    :operator (get tokens i)}
                   new-index))
          [e i])))))

(def factor (binary-matcher #{:slash :star} unary))
(def term (binary-matcher #{:minus :plus} factor))
(def comparison (binary-matcher #{:greater :greater-equal :less :less-equal} term))
(def equality (binary-matcher #{:bang-equal :equal-equal} comparison))

(defn- expression [tokens token-index]
  (equality tokens token-index))

(defn parse
  "Takes tokens and returns an AST of expressions."
  [tokens]
  (f/ok-> (expression tokens 0)
          first))
