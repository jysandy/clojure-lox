(ns clojure-lox.interpreter-test
  (:require [clojure.test :refer :all]
            [clojure-lox.interpreter :as interpreter]
            [clojure-lox.expression :as expr]
            [clojure-lox.statement :as stmt]
            [clojure-lox.environment :as environment]))

(deftest evaluate-test
  (is (= "foobar"
         (interpreter/evaluate {:type     ::expr/binary,
                                :left     {:type ::expr/literal, :value "foo"},
                                :right    {:type ::expr/literal, :value "bar"},
                                :operator {:token-type :plus, :literal nil, :lexeme "+", :line 1}})))

  (is (= 5.0
         (interpreter/evaluate {:type     ::expr/binary,
                                :left     {:type ::expr/literal, :value 2.0},
                                :right    {:type ::expr/literal, :value 3.0},
                                :operator {:token-type :plus, :literal nil, :lexeme "+", :line 1}})))

  (is (= 18.0
         (interpreter/evaluate {:type     ::expr/binary,
                                :left     {:type ::expr/literal, :value 3.0},
                                :right    {:type       ::expr/grouping,
                                           :expression {:type     ::expr/binary,
                                                        :left     {:type ::expr/literal, :value 2.0},
                                                        :right    {:type ::expr/literal, :value 4.0},
                                                        :operator {:token-type :plus, :literal nil, :lexeme "+", :line 1}}},
                                :operator {:token-type :star, :literal nil, :lexeme "*", :line 1}})))

  (is (= false
         (interpreter/evaluate {:type     ::expr/binary,
                                :left     {:type ::expr/literal, :value 2.0},
                                :right    {:type ::expr/literal, :value 3.0},
                                :operator {:token-type :greater, :literal nil, :lexeme ">", :line 1}})))

  (is (= true
         (interpreter/evaluate {:type     ::expr/binary,
                                :left     {:type ::expr/literal, :value 2.0},
                                :right    {:type ::expr/literal, :value 3.0},
                                :operator {:token-type :less-equal, :literal nil, :lexeme "<=", :line 1}})))

  (is (= true
         (interpreter/evaluate {:type     ::expr/binary,
                                :left     {:type ::expr/literal, :value 4.0},
                                :right    {:type ::expr/literal, :value 4.0},
                                :operator {:token-type :equal-equal, :literal nil, :lexeme "==", :line 1}})))

  (is (= true
         (interpreter/evaluate {:type     ::expr/binary,
                                :left     {:type ::expr/literal, :value 4.0},
                                :right    {:type ::expr/literal, :value 5.0},
                                :operator {:token-type :bang-equal, :literal nil, :lexeme "!=", :line 1}})))

  (is (= true
         (interpreter/evaluate {:type     ::expr/binary,
                                :left     {:type ::expr/literal, :value 4.0},
                                :right    {:type ::expr/literal, :value 5.0},
                                :operator {:token-type :bang-equal, :literal nil, :lexeme "!=", :line 1}})))

  (is (= nil
         (interpreter/evaluate {:type       ::stmt/expression
                                :expression {:type     ::expr/binary,
                                             :left     {:type ::expr/literal, :value 4.0},
                                             :right    {:type ::expr/literal, :value 5.0},
                                             :operator {:token-type :bang-equal, :literal nil, :lexeme "!=", :line 1}}})))

  (is (= "5.0\n"
         (with-out-str
           (interpreter/evaluate {:type       ::stmt/print
                                  :expression {:type     ::expr/binary,
                                               :left     {:type ::expr/literal, :value 2.0},
                                               :right    {:type ::expr/literal, :value 3.0},
                                               :operator {:token-type :plus, :literal nil, :lexeme "+", :line 1}}}))))

  (do (interpreter/evaluate {:type        ::stmt/var
                             :name        {:token-type :identifier
                                           :lexeme     "foo"}
                             :initializer {:type     ::expr/binary,
                                           :left     {:type ::expr/literal, :value 2.0},
                                           :right    {:type ::expr/literal, :value 3.0},
                                           :operator {:token-type :plus, :literal nil, :lexeme "+", :line 1}}})
      (is (= 5.0
             (environment/get "foo")))
      (environment/clear!))

  (do
    (environment/define "foo" 6.0)
    (is (= 6.0 (interpreter/evaluate {:type ::expr/variable
                                      :name {:token-type :identifier
                                             :lexeme     "foo"}})))
    (environment/clear!))

  (do
    (is (= (interpreter/runtime-error {:token-type :identifier
                                       :lexeme     "foo"
                                       :line       1}
                                      "Undefined variable 'foo'.")
           (interpreter/evaluate {:type ::expr/variable
                                  :name {:token-type :identifier
                                         :lexeme     "foo"
                                         :line       1}})))
    (environment/clear!)))


