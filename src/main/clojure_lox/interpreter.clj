(ns clojure-lox.interpreter
  (:require [clojure-lox.expression :as expr]
            [clojure-lox.statement :as stmt]
            [clojure-lox.environment :as environment]
            [failjure.core :as f]
            [clojure-lox.error :as e]))

(def error (partial e/error ::error))

(defn runtime-error
  [token message]
  (error (:line token) message))

(defmulti evaluate
          "Evaluates an expression or statement."
          :type)

(defmethod evaluate ::expr/literal
  [{:keys [value]}]
  value)

(defmethod evaluate ::expr/grouping
  [{:keys [expression]}]
  (evaluate expression))

(defmethod evaluate ::expr/unary
  [{:keys [operator right]}]
  (f/attempt-all [right-value (evaluate right)]
    (case (:token-type operator)
      :minus (if (number? right-value)
               (- right-value)
               (runtime-error operator "Operand must be a number"))
      :bang (not right-value))))

(defmethod evaluate ::expr/binary
  [{:keys [left right operator]}]
  (f/attempt-all [left-value           (evaluate left)
                  right-value          (evaluate right)
                  operands-are-numbers (and (number? left-value)
                                            (number? right-value))
                  apply-numerical-operator (fn [op-fn]
                                             (if operands-are-numbers
                                               (op-fn left-value right-value)
                                               (runtime-error operator "Operands must be numbers")))]
    (case (:token-type operator)
      :plus (cond
              operands-are-numbers
              (+ left-value right-value)

              (and (string? left-value)
                   (string? right-value))
              (str left-value right-value)

              :else
              (runtime-error operator "Operands must be numbers or strings"))
      :minus (apply-numerical-operator -)
      :star (apply-numerical-operator *)
      :slash (f/ok-> (apply-numerical-operator /)
                     double)
      :greater (apply-numerical-operator >)
      :greater-equal (apply-numerical-operator >=)
      :less (apply-numerical-operator <)
      :less-equal (apply-numerical-operator <=)

      :bang-equal (not= left-value right-value)
      :equal-equal (= left-value right-value))))

(defmethod evaluate ::stmt/expression
  [{:keys [expression]}]
  (evaluate expression)
  nil)

(defmethod evaluate ::stmt/print
  [{:keys [expression]}]
  (f/attempt-all [value (evaluate expression)]
    (println value)))

(defmethod evaluate ::stmt/var
  [{:keys [initializer]
    name-token :name}]
  (f/attempt-all [value (when initializer
                          (evaluate initializer))]
    (do (environment/define (:lexeme name-token) value)
        nil)))

(defmethod evaluate ::expr/variable
  [{name-token :name}]
  (f/if-let-ok? [value (environment/get (:lexeme name-token))]
    value
    (runtime-error name-token
                   (format "Undefined variable '%s'." (:lexeme name-token)))))

(defn interpret [statements]
  (e/first-failure evaluate statements))

;; TODO: https://craftinginterpreters.com/statements-and-state.html#assignment
