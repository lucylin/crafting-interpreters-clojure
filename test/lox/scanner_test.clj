(ns lox.scanner-test
  (:require [clojure.test :refer [deftest is testing are]]
            [lox.scanner :as scanner]))

(defn- check-token-ok
  [input]
  (let [result (scanner/scan-tokens input)]
    (is (empty? (:errors result)))
    (butlast (:tokens result))))

(defn- count-tokens-errors
  [input]
  (let [result (scanner/scan-tokens input)]
    {:num-tokens (dec (count (:tokens result)))  ; remove EOF token
     :num-errors (count (:errors result))}))

(deftest test-basic
  (testing "end EOF"
    (is (= {:tokens [{:token :eof :lexeme nil :literal nil :line 1}] :errors []}
           (scanner/scan-tokens ""))))
  (testing "invalid character"
    (is (= {:num-tokens 0 :num-errors 2}
           (count-tokens-errors "^$"))))
  (testing "unterminated string literal"
    (is (= {:num-tokens 0 :num-errors 1}
           (count-tokens-errors "\"...")))))

(deftest test-single-char
  (are [input expected] (= expected (check-token-ok input))
    "(" [{:token :left-paren :lexeme "(" :literal nil :line 1}]
    ")" [{:token :right-paren :lexeme ")" :literal nil  :line 1}]
    "{" [{:token :left-brace :lexeme "{" :literal nil :line 1}]
    "}" [{:token :right-brace :lexeme "}" :literal nil :line 1}]
    "," [{:token :comma :lexeme "," :literal nil :line 1}]
    "." [{:token :dot :lexeme "." :literal nil :line 1}]
    "-" [{:token :minus :lexeme "-" :literal nil :line 1}]
    "+" [{:token :plus :lexeme "+" :literal nil :line 1}]
    ";" [{:token :semicolon :lexeme ";" :literal nil :line 1}]
    "*" [{:token :star :lexeme "*" :literal nil :line 1}]))

(deftest test-equals-op
  (are [input expected] (= expected (check-token-ok input))
    "!!" [{:token :bang :lexeme "!" :literal nil :line 1}
          {:token :bang :lexeme "!" :literal nil :line 1}]
    "!=" [{:token :bang-equal :lexeme "!=" :literal nil :line 1}]
    "=." [{:token :equal :lexeme "=" :literal nil :line 1}
          {:token :dot :lexeme "." :literal nil :line 1}]
    "==" [{:token :equal-equal :lexeme "==" :literal nil :line 1}]
    ">>" [{:token :greater :lexeme ">" :literal nil :line 1}
          {:token :greater :lexeme ">" :literal nil :line 1}]
    ">=" [{:token :greater-equal :lexeme ">=" :literal nil :line 1}]
    "<<" [{:token :less :lexeme "<" :literal nil :line 1}
          {:token :less :lexeme "<" :literal nil :line 1}]
    "<=" [{:token :less-equal :lexeme "<=" :literal nil :line 1}]))

(deftest test-slash-comment
  (are [input expected] (= expected (check-token-ok input))
    "/." [{:token :slash :lexeme "/" :literal nil :line 1}
          {:token :dot :lexeme "." :literal nil :line 1}]
    "// /xyz\n /" [{:token :slash :lexeme "/" :literal nil :line 2}]))

(deftest test-string-literal
  (are [input expected] (= expected (check-token-ok input))
    "\"hello world\n1\n\"!" [{:token :string :lexeme "\"hello world\n1\n\""
                              :literal "hello world\n1\n" :line 3}
                             {:token :bang :lexeme "!" :literal nil :line 3}]
    "\"// abc\"" [{:token :string :lexeme "\"// abc\""
                   :literal "// abc" :line 1}]))

(deftest test-number-literal
  (are [input expected] (= expected (check-token-ok input))
    "1234." [{:token :number :lexeme "1234" :literal 1234.0 :line 1}
             {:token :dot :lexeme "." :literal nil :line 1}]
    ".1234" [{:token :dot :lexeme "." :literal nil :line 1}
             {:token :number :lexeme "1234" :literal 1234.0 :line 1}]
    "12.34" [{:token :number :lexeme "12.34" :literal 12.34 :line 1}]))

(deftest test-reserved-identifier
  (are [input expected] (= expected (check-token-ok input))
    "and" [{:token :and :lexeme "and" :literal nil :line 1}]
    "class" [{:token :class :lexeme "class" :literal nil :line 1}]
    "else"  [{:token :else :lexeme "else" :literal nil :line 1}]
    "false" [{:token :false :lexeme "false" :literal nil :line 1}]
    "for" [{:token :for :lexeme "for" :literal nil :line 1}]
    "forever" [{:token :identifier :lexeme "forever" :literal nil :line 1}]
    "fun" [{:token :fun :lexeme "fun" :literal nil :line 1}]
    "if"  [{:token :if :lexeme "if" :literal nil :line 1}]
    "nil" [{:token :nil :lexeme "nil" :literal nil :line 1}]
    "or"  [{:token :or :lexeme "or" :literal nil :line 1}]
    "print" [{:token :print :lexeme "print" :literal nil :line 1}]
    "return"  [{:token :return :lexeme "return" :literal nil :line 1}]
    "super" [{:token :super :lexeme "super" :literal nil :line 1}]
    "this" [{:token :this :lexeme "this" :literal nil :line 1}]
    "true" [{:token :true :lexeme "true" :literal nil :line 1}]
    "var" [{:token :var :lexeme "var" :literal nil :line 1}]
    "while" [{:token :while :lexeme "while" :literal nil :line 1}]
    "meanwhile" [{:token :identifier :lexeme "meanwhile" :literal nil :line 1}]))

; TODO:: check mixed tokens & errors

(comment
  (clojure.test/run-tests 'lox.scanner-test))
