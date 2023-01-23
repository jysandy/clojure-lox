(ns clojure-lox.parser-test
  (:require [clojure.test :refer :all]
            [clojure-lox.parser :as parser]))

(deftest parse-test
  (is (= {:type  :clojure-lox.expression/literal
          :value 2.0}
         (parser/parse [{:token-type :number, :literal 2.0, :lexeme "2", :line 1}
                        {:token-type :eof, :literal nil, :lexeme nil, :line 1}])))

  (is (= (parser/error 1 "at '+': Expect expression")
         (parser/parse [{:token-type :plus, :literal nil, :lexeme "+", :line 1}
                        {:token-type :eof, :literal nil, :lexeme nil, :line 1}])))

  (is (= {:type     :clojure-lox.expression/binary,
          :left     {:type :clojure-lox.expression/literal, :value 2.0},
          :right    {:type :clojure-lox.expression/literal, :value 3.0},
          :operator {:token-type :plus, :literal nil, :lexeme "+", :line 1}}
         (parser/parse [{:token-type :number, :literal 2.0, :lexeme "2", :line 1}
                        {:token-type :plus, :literal nil, :lexeme "+", :line 1}
                        {:token-type :number, :literal 3.0, :lexeme "3", :line 1}
                        {:token-type :eof, :literal nil, :lexeme nil, :line 1}])))

  (is (= {:type     :clojure-lox.expression/binary,
          :left     {:type     :clojure-lox.expression/binary,
                     :left     {:type :clojure-lox.expression/literal, :value 2.0},
                     :right    {:type :clojure-lox.expression/literal, :value 3.0},
                     :operator {:token-type :plus, :literal nil, :lexeme "+", :line 1}},
          :right    {:type     :clojure-lox.expression/binary,
                     :left     {:type :clojure-lox.expression/literal, :value 1.0},
                     :right    {:type :clojure-lox.expression/literal, :value 4.5},
                     :operator {:token-type :slash, :literal nil, :lexeme "/", :line 1}},
          :operator {:token-type :equal-equal, :literal nil, :lexeme "==", :line 1}}
         (parser/parse [{:token-type :number, :literal 2.0, :lexeme "2", :line 1}
                        {:token-type :plus, :literal nil, :lexeme "+", :line 1}
                        {:token-type :number, :literal 3.0, :lexeme "3", :line 1}
                        {:token-type :equal-equal, :literal nil, :lexeme "==", :line 1}
                        {:token-type :number, :literal 1.0, :lexeme "1", :line 1}
                        {:token-type :slash, :literal nil, :lexeme "/", :line 1}
                        {:token-type :number, :literal 4.5, :lexeme "4.5", :line 1}
                        {:token-type :eof, :literal nil, :lexeme nil, :line 1}])))

  (is (= {:type     :clojure-lox.expression/binary,
          :left     {:type     :clojure-lox.expression/binary,
                     :left     {:type       :clojure-lox.expression/grouping,
                                :expression {:type     :clojure-lox.expression/binary,
                                             :left     {:type :clojure-lox.expression/literal, :value 2.0},
                                             :right    {:type :clojure-lox.expression/literal, :value 3.0},
                                             :operator {:token-type :plus, :literal nil, :lexeme "+", :line 1}}},
                     :right    {:type :clojure-lox.expression/literal, :value 6.0},
                     :operator {:token-type :star, :literal nil, :lexeme "*", :line 1}},
          :right    {:type     :clojure-lox.expression/binary,
                     :left     {:type :clojure-lox.expression/literal, :value 1.0},
                     :right    {:type :clojure-lox.expression/literal, :value 4.5},
                     :operator {:token-type :slash, :literal nil, :lexeme "/", :line 1}},
          :operator {:token-type :greater-equal, :literal nil, :lexeme ">=", :line 1}}
         (parser/parse [{:token-type :left-paren, :literal nil, :lexeme "(", :line 1}
                        {:token-type :number, :literal 2.0, :lexeme "2", :line 1}
                        {:token-type :plus, :literal nil, :lexeme "+", :line 1}
                        {:token-type :number, :literal 3.0, :lexeme "3", :line 1}
                        {:token-type :right-paren, :literal nil, :lexeme ")", :line 1}
                        {:token-type :star, :literal nil, :lexeme "*", :line 1}
                        {:token-type :number, :literal 6.0, :lexeme "6", :line 1}
                        {:token-type :greater-equal, :literal nil, :lexeme ">=", :line 1}
                        {:token-type :number, :literal 1.0, :lexeme "1", :line 1}
                        {:token-type :slash, :literal nil, :lexeme "/", :line 1}
                        {:token-type :number, :literal 4.5, :lexeme "4.5", :line 1}
                        {:token-type :eof, :literal nil, :lexeme nil, :line 1}])))

  (is (= {:type     :clojure-lox.expression/binary,
          :left     {:type       :clojure-lox.expression/grouping,
                     :expression {:type     :clojure-lox.expression/binary,
                                  :left     {:type :clojure-lox.expression/literal, :value 2.0},
                                  :right    {:type     :clojure-lox.expression/unary,
                                             :operator {:token-type :minus, :literal nil, :lexeme "-", :line 1},
                                             :right    {:type :clojure-lox.expression/literal, :value 3.0}},
                                  :operator {:token-type :plus, :literal nil, :lexeme "+", :line 1}}},
          :right    {:type :clojure-lox.expression/literal, :value 6.0},
          :operator {:token-type :star, :literal nil, :lexeme "*", :line 1}}
         (parser/parse [{:token-type :left-paren, :literal nil, :lexeme "(", :line 1}
                        {:token-type :number, :literal 2.0, :lexeme "2", :line 1}
                        {:token-type :plus, :literal nil, :lexeme "+", :line 1}
                        {:token-type :minus, :literal nil, :lexeme "-", :line 1}
                        {:token-type :number, :literal 3.0, :lexeme "3", :line 1}
                        {:token-type :right-paren, :literal nil, :lexeme ")", :line 1}
                        {:token-type :star, :literal nil, :lexeme "*", :line 1}
                        {:token-type :number, :literal 6.0, :lexeme "6", :line 1}
                        {:token-type :eof, :literal nil, :lexeme nil, :line 1}])))

  (is (= (parser/error 2 "at end: Expect expression")
         (parser/parse [{:token-type :number, :literal 2.0, :lexeme "2", :line 1}
                        {:token-type :plus, :literal nil, :lexeme "+", :line 1}
                        {:token-type :number, :literal 3.0, :lexeme "3", :line 1}
                        {:token-type :greater-equal, :literal nil, :lexeme ">=", :line 2}
                        {:token-type :eof, :literal nil, :lexeme nil, :line 2}])))

  (is (= (parser/error 1 "at end: Expect ')' after expression.")
         (parser/parse [{:token-type :number, :literal 2.0, :lexeme "2", :line 1}
                        {:token-type :plus, :literal nil, :lexeme "+", :line 1}
                        {:token-type :number, :literal 3.0, :lexeme "3", :line 1}
                        {:token-type :greater-equal, :literal nil, :lexeme ">=", :line 1}
                        {:token-type :left-paren, :literal nil, :lexeme "(", :line 1}
                        {:token-type :number, :literal 4.0, :lexeme "4", :line 1}
                        {:token-type :eof, :literal nil, :lexeme nil, :line 1}])))

  (is (= (parser/error 1 "at 'abc': Expect expression")
         (parser/parse [{:token-type :minus, :literal nil, :lexeme "-", :line 1}
                        {:token-type :identifier, :literal nil, :lexeme "abc", :line 1}
                        {:token-type :eof, :literal nil, :lexeme nil, :line 1}]))))
