(ns lox.scanner-test
  (:require [clojure.test :refer [deftest is testing are]]
            [lox.scanner :as scanner]
            [lox.test-util :as util]))

(defn- check-token-ok [input] (butlast (util/scan-ok input)))

(defn- check-token-lex-ok
  [input]
  (let [tokens (util/scan-ok input)]
    (butlast (map #(dissoc % :line :literal) tokens))))

(defn- count-tokens-errors
  [input]
  (let [result (scanner/scan-tokens input)]
    {:num-tokens (dec (count (:tokens result)))  ; remove EOF token
     :num-errors (count (:errors result))}))

(deftest test-basic
  (testing "end EOF"
    (is (= {:tokens [{:type :eof :lexeme nil :literal nil :line 1}] :errors []}
           (scanner/scan-tokens ""))))
  (testing "invalid character"
    (is (= {:num-tokens 0 :num-errors 2}
           (count-tokens-errors "^$"))))
  (testing "unterminated string literal"
    (is (= {:num-tokens 0 :num-errors 1}
           (count-tokens-errors "\"..."))))
  (testing "mixed tokens and errors"
    (is (= {:tokens [{:type :bang, :lexeme "!", :literal nil, :line 2}
                     {:type :eof, :lexeme nil, :literal nil, :line 2}]
            :errors [{:message "unexpected character $", :line 1}
                     {:message "unexpected character #", :line 1}
                     {:message "unexpected character ^", :line 2}
                     {:message "unexpected character &", :line 2}]}
           (scanner/scan-tokens "$#\n^&!")))))

(deftest test-single-char
  (are [input expected] (= expected (check-token-lex-ok input))
    "(" [{:type :left-paren :lexeme "("}]
    ")" [{:type :right-paren :lexeme ")"}]
    "{" [{:type :left-brace :lexeme "{"}]
    "}" [{:type :right-brace :lexeme "}"}]
    "," [{:type :comma :lexeme ","}]
    "." [{:type :dot :lexeme "."}]
    "-" [{:type :minus :lexeme "-"}]
    "+" [{:type :plus :lexeme "+"}]
    ";" [{:type :semicolon :lexeme ";"}]
    "*" [{:type :star :lexeme "*"}]))

(deftest test-equals-op
  (are [input expected] (= expected (check-token-lex-ok input))
    "!!" [{:type :bang :lexeme "!"}
          {:type :bang :lexeme "!"}]
    "!=" [{:type :bang-equal :lexeme "!="}]
    "=." [{:type :equal :lexeme "="}
          {:type :dot :lexeme "."}]
    "==" [{:type :equal-equal :lexeme "=="}]
    ">>" [{:type :greater :lexeme ">"}
          {:type :greater :lexeme ">"}]
    ">=" [{:type :greater-equal :lexeme ">="}]
    "<<" [{:type :less :lexeme "<"}
          {:type :less :lexeme "<"}]
    "<=" [{:type :less-equal :lexeme "<="}]))

(deftest test-slash-comment
  (are [input expected] (= expected (check-token-ok input))
    "/." [{:type :slash :lexeme "/" :literal nil :line 1}
          {:type :dot :lexeme "." :literal nil :line 1}]
    "// /xyz\n /" [{:type :slash :lexeme "/" :literal nil :line 2}]))

(deftest test-string-literal
  (are [input expected] (= expected (check-token-ok input))
    "\"hello world\n1\n\"!" [{:type :string :lexeme "\"hello world\n1\n\""
                              :literal "hello world\n1\n" :line 3}
                             {:type :bang :lexeme "!" :literal nil :line 3}]
    "\"// abc\"" [{:type :string :lexeme "\"// abc\""
                   :literal "// abc" :line 1}]))

(deftest test-number-literal
  (are [input expected] (= expected (check-token-ok input))
    "1234." [{:type :number :lexeme "1234" :literal 1234.0 :line 1}
             {:type :dot :lexeme "." :literal nil :line 1}]
    ".1234" [{:type :dot :lexeme "." :literal nil :line 1}
             {:type :number :lexeme "1234" :literal 1234.0 :line 1}]
    "12.34" [{:type :number :lexeme "12.34" :literal 12.34 :line 1}]))

(deftest test-reserved
  (are [input expected] (= expected (check-token-lex-ok input))
    "and" [{:type :and :lexeme "and"}]
    "class" [{:type :class :lexeme "class"}]
    "else" [{:type :else :lexeme "else"}]
    "false" [{:type :false :lexeme "false"}]
    "for" [{:type :for :lexeme "for"}]
    "fun" [{:type :fun :lexeme "fun"}]
    "if" [{:type :if :lexeme "if"}]
    "nil" [{:type :nil :lexeme "nil"}]
    "or" [{:type :or :lexeme "or"}]
    "print" [{:type :print :lexeme "print"}]
    "return" [{:type :return :lexeme "return"}]
    "super" [{:type :super :lexeme "super"}]
    "this" [{:type :this :lexeme "this"}]
    "true" [{:type :true :lexeme "true"}]
    "var" [{:type :var :lexeme "var"}]
    "while" [{:type :while :lexeme "while"}]))

(deftest test-identifier
  (are [input expected] (= expected (check-token-ok input))
    "forever" [{:type :identifier :lexeme "forever" :literal nil :line 1}]
    "meanwhile" [{:type :identifier :lexeme "meanwhile" :literal nil :line 1}]))

(comment
  (clojure.test/run-tests 'lox.scanner-test))
