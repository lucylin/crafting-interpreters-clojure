(ns lox.scanner-test
  (:require [clojure.test :refer [deftest is testing are]]
            [lox.scanner :as scanner]))

(defn- check-token-helper
  [input]
  (butlast (:tokens (scanner/scan-tokens input))))

(defn- check-error-count
  [input]
  (count (:errors (scanner/scan-tokens input))))

(deftest test-basic
  (testing "end EOF"
    (is (= {:tokens [{:type :eof :lexeme nil :literal nil :line 1}] :errors []}
           (scanner/scan-tokens ""))))
  (testing "invalid character"
    (is (and (= 1 (check-error-count "^"))
             (= 0 (count (check-token-helper "^")))))))

(deftest test-single-char
  (are [input expected] (= expected (check-token-helper input))
    "(" [{:type :left-paren :lexeme "(" :literal nil :line 1}]
    ")" [{:type :right-paren :lexeme ")" :literal nil  :line 1}]
    "{" [{:type :left-brace :lexeme "{" :literal nil :line 1}]
    "}" [{:type :right-brace :lexeme "}" :literal nil :line 1}]
    "," [{:type :comma :lexeme "," :literal nil :line 1}]
    "." [{:type :dot :lexeme "." :literal nil :line 1}]
    "-" [{:type :minus :lexeme "-" :literal nil :line 1}]
    "+" [{:type :plus :lexeme "+" :literal nil :line 1}]
    ";" [{:type :semicolon :lexeme ";" :literal nil :line 1}]
    "*" [{:type :star :lexeme "*" :literal nil :line 1}]))

(deftest test-equals-op
  (are [input expected] (= expected (check-token-helper input))
    "!!" [{:type :bang :lexeme "!" :literal nil :line 1}
          {:type :bang :lexeme "!" :literal nil :line 1}]
    "!=" [{:type :bang-equal :lexeme "!=" :literal nil :line 1}]
    "=." [{:type :equal :lexeme "=" :literal nil :line 1}
          {:type :dot :lexeme "." :literal nil :line 1}]
    "==" [{:type :equal-equal :lexeme "==" :literal nil :line 1}]
    ">>" [{:type :greater :lexeme ">" :literal nil :line 1}
          {:type :greater :lexeme ">" :literal nil :line 1}]
    ">=" [{:type :greater-equal :lexeme ">=" :literal nil :line 1}]
    "<<" [{:type :less :lexeme "<" :literal nil :line 1}
          {:type :less :lexeme "<" :literal nil :line 1}]
    "<=" [{:type :less-equal :lexeme "<=" :literal nil :line 1}]))

(deftest test-slash-comment
  (are [input expected] (= expected (check-token-helper input))
    "/." [{:type :slash :lexeme "/" :literal nil :line 1}
          {:type :dot :lexeme "." :literal nil :line 1}]
    "// /xyz\n /" [{:type :slash :lexeme "/" :literal nil :line 2}]))

(deftest test-string-literal
  ; TODO: test unterminated string
  (are [input expected] (= expected (check-token-helper input))
    "\"hello world\n1\n\"!" [{:type :string :lexeme "\"hello world\n1\n\""
                              :literal "hello world\n1\n" :line 3}
                             {:type :bang :lexeme "!" :literal nil :line 3}]))

(deftest test-number-literal
  (are [input expected] (= expected (check-token-helper input))
    "1234." [{:type :number :lexeme "1234" :literal 1234.0 :line 1}
             {:type :dot :lexeme "." :literal nil :line 1}]
    ".1234" [{:type :dot :lexeme "." :literal nil :line 1}
             {:type :number :lexeme "1234" :literal 1234.0 :line 1}]
    "12.34" [{:type :number :lexeme "12.34" :literal 12.34 :line 1}]))

(deftest test-reserved-identifier
  (are [input expected] (= expected (check-token-helper input))
    "and" [{:type :and :lexeme "and" :literal nil :line 1}]
    "class" [{:type :class :lexeme "class" :literal nil :line 1}]
    "else"  [{:type :else :lexeme "else" :literal nil :line 1}]
    "false" [{:type :false :lexeme "false" :literal nil :line 1}]
    "for" [{:type :for :lexeme "for" :literal nil :line 1}]
    "forever" [{:type :identifier :lexeme "forever" :literal nil :line 1}]
    "fun" [{:type :fun :lexeme "fun" :literal nil :line 1}]
    "if"  [{:type :if :lexeme "if" :literal nil :line 1}]
    "nil" [{:type :nil :lexeme "nil" :literal nil :line 1}]
    "or"  [{:type :or :lexeme "or" :literal nil :line 1}]
    "print" [{:type :print :lexeme "print" :literal nil :line 1}]
    "return"  [{:type :return :lexeme "return" :literal nil :line 1}]
    "super" [{:type :super :lexeme "super" :literal nil :line 1}]
    "this" [{:type :this :lexeme "this" :literal nil :line 1}]
    "true" [{:type :true :lexeme "true" :literal nil :line 1}]
    "var" [{:type :var :lexeme "var" :literal nil :line 1}]
    "while" [{:type :while :lexeme "while" :literal nil :line 1}]
    "meanwhile" [{:type :identifier :lexeme "meanwhile" :literal nil :line 1}]))

; TODO:: check mixed tokens & errors

(comment
  (clojure.test/run-tests 'lox.scanner-test))
