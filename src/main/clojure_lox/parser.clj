(ns clojure-lox.parser
  (:require [clojure-lox.expression :as expression]
            [clojure-lox.statement :as statement]
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

(defn- at-end? [tokens token-index]
  (or (>= token-index (count tokens))
      (= :eof (:token-type (get tokens token-index)))))

(defn- consume [tokens token-index token-type error-msg]
  (if (= token-type (:token-type (get tokens token-index)))
    [(get tokens token-index) (inc token-index)]
    (token-error (get tokens token-index) error-msg)))

(defn- primary [tokens token-index]
  (condp token-matches? (get tokens token-index)
    #{:false} [{:type ::expression/literal :value false} (inc token-index)]
    #{:true} [{:type ::expression/literal :value true} (inc token-index)]
    #{:nil} [{:type ::expression/literal :value nil} (inc token-index)]
    #{:number :string} [{:type ::expression/literal :value (:literal (get tokens token-index))}
                        (inc token-index)]
    #{:identifier} [{:type ::expression/variable :name (get tokens token-index)}
                    (inc token-index)]
    #{:left-paren} (f/attempt-all [expr-and-new-index (expression tokens (inc token-index))
                                   expr               (first expr-and-new-index)
                                   new-index          (f/ok-> (consume tokens
                                                                       (second expr-and-new-index)
                                                                       :right-paren
                                                                       "Expect ')' after expression.")
                                                              second)]
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

(defn- print-statement [tokens token-index]
  (f/attempt-all [expr-and-new-index (expression tokens token-index)
                  [expr new-index] expr-and-new-index
                  newer-index        (f/ok-> (consume tokens new-index :semicolon "Expect ';' after value.")
                                             second)]
    [{:type       ::statement/print
      :expression expr}
     newer-index]))

(defn- expression-statement [tokens token-index]
  (f/attempt-all [expr-and-new-index (expression tokens token-index)
                  [expr new-index] expr-and-new-index
                  newer-index        (f/ok-> (consume tokens new-index :semicolon "Expect ';' after expression.")
                                             second)]
    [{:type       ::statement/expression
      :expression expr}
     newer-index]))

(defn- statement [tokens token-index]
  (if (token-matches? #{:print} (get tokens token-index))
    (print-statement tokens (inc token-index))
    (expression-statement tokens token-index)))

(defn- var-declaration [tokens token-index]
  (f/attempt-all [name-and-new-index        (consume tokens token-index :identifier "Expect variable name.")
                  [var-name new-index] name-and-new-index
                  initializer-and-new-index (if (token-matches? #{:equal} (get tokens new-index))
                                              (expression tokens (inc new-index))
                                              [nil new-index])
                  [initializer new-index] initializer-and-new-index
                  token-and-new-index       (consume tokens new-index :semicolon "Expect ';' after variable declaration.")
                  [_ new-index] token-and-new-index]
    [{:type        ::statement/var
      :name        var-name
      :initializer initializer}
     new-index]))

(defn- declaration [tokens token-index]
  (f/attempt-all [ast-and-index (if (token-matches? #{:var} (get tokens token-index))
                                  (var-declaration tokens (inc token-index))
                                  (statement tokens token-index))]
    ast-and-index))

(defn- synchronize [tokens token-index]
  (loop [i (inc token-index)]
    (cond
      (at-end? tokens i)
      i

      (= :semicolon (:token-type (get tokens (dec i))))
      i

      (#{:class :fun :var :for :if :while :print :return}
       (:token-type (get tokens i)))
      i

      :else
      (recur (inc i)))))

(defn parse
  "Takes tokens and returns an AST of statements."
  [tokens]
  (loop [statements  []
         token-index 0
         errors      []]
    (if (at-end? tokens token-index)
      (if (seq errors)
        (e/fail {:type   ::parsing-failed
                 :errors errors})
        statements)
      (f/if-let-ok? [result (declaration tokens token-index)]
        (let [[statement new-index] result]
          (recur (conj statements statement) new-index errors))
        (if (= (namespace (:type result)) "clojure-lox.parser")
          (recur statements
                 (synchronize tokens token-index)
                 (conj errors result))
          result)))))
