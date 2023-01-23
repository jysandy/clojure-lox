(ns clojure-lox.interpreter-test
  (:require [clojure.test :refer :all]
            [clojure-lox.interpreter :as interpreter]
            [clojure-lox.expression :as expr]))

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
                                :operator {:token-type :bang-equal, :literal nil, :lexeme "!=", :line 1}}))))
