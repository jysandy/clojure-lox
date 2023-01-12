(ns clojure-lox.scanner-test
  (:require [clojure.test :refer :all]
            [clojure-lox.scanner :as scanner]))

(deftest scan-tokens-test
  (is (= [[{:token-type :left-brace, :literal nil, :lexeme "{", :line 1}
           {:token-type :star, :literal nil, :lexeme "*", :line 1}
           {:token-type :greater-equal, :literal nil, :lexeme ">=", :line 1}
           {:token-type :right-paren, :literal nil, :lexeme ")", :line 2}
           {:token-type :left-paren, :literal nil, :lexeme "(", :line 2}
           {:token-type :identifier, :literal nil, :lexeme "bar", :line 2}
           {:token-type :eof, :literal nil, :lexeme nil, :line 2}]
          [{:line 1, :message "invalid character @"}]]
         (scanner/scan-tokens "{*@>=//}foo\n)(bar")))
  (is (= [[{:token-type :string, :literal "fo\no", :lexeme "\"fo\no\"", :line 1}
           {:token-type :left-paren, :literal nil, :lexeme "(", :line 2}
           {:token-type :right-paren, :literal nil, :lexeme ")", :line 2}
           {:token-type :eof, :literal nil, :lexeme nil, :line 2}]
          []]
         (scanner/scan-tokens "\"fo\no\"()")))
  (is (= [[{:token-type :string, :literal "foo", :lexeme "\"foo\"", :line 1}
           {:token-type :eof, :literal nil, :lexeme nil, :line 1}]
          []]
         (scanner/scan-tokens "\"foo\"")))
  (is (= [[{:token-type :eof, :literal nil, :lexeme nil, :line 2}]
          [{:line 2, :message "unterminated string"}]]
         (scanner/scan-tokens "\"fo\no()")))
  (is (= [[{:token-type :number, :literal 123.0, :lexeme "123", :line 1} {:token-type :eof, :literal nil, :lexeme nil, :line 1}]
          []]
         (scanner/scan-tokens "123")))
  (is (= [[{:token-type :number, :literal 123.45, :lexeme "123.45", :line 1} {:token-type :eof, :literal nil, :lexeme nil, :line 1}]
          []]
         (scanner/scan-tokens "123.45")))
  (is (= [[{:token-type :number, :literal 123.0, :lexeme "123", :line 1}
           {:token-type :dot, :literal nil, :lexeme ".", :line 1}
           {:token-type :eof, :literal nil, :lexeme nil, :line 1}]
          []]
         (scanner/scan-tokens "123.")))
  (is (= [[{:token-type :number, :literal 123.0, :lexeme "123", :line 1}
           {:token-type :dot, :literal nil, :lexeme ".", :line 1}
           {:token-type :greater-equal, :literal nil, :lexeme ">=", :line 1}
           {:token-type :eof, :literal nil, :lexeme nil, :line 1}]
          []]
         (scanner/scan-tokens "123.>=")))
  (is (= [[{:token-type :number, :literal 123.45, :lexeme "123.45", :line 1}
           {:token-type :greater-equal, :literal nil, :lexeme ">=", :line 1}
           {:token-type :eof, :literal nil, :lexeme nil, :line 1}]
          []]
         (scanner/scan-tokens "123.45>=")))
  (is (= [[{:token-type :number, :literal 123.45, :lexeme "123.45", :line 1}
           {:token-type :number, :literal 12.0, :lexeme "12", :line 1}
           {:token-type :identifier, :literal nil, :lexeme "_aasd32e4bcif", :line 1}
           {:token-type :if, :literal nil, :lexeme "if", :line 1}
           {:token-type :or, :literal nil, :lexeme "or", :line 1}
           {:token-type :and, :literal nil, :lexeme "and", :line 2}
           {:token-type :while, :literal nil, :lexeme "while", :line 2}
           {:token-type :eof, :literal nil, :lexeme nil, :line 2}]
          []]
         (scanner/scan-tokens "123.45 12_aasd32e4bcif if or\n and while"))))
