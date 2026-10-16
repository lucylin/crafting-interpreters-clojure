(ns lox.scanner-test
  (:require [clojure.test :refer [deftest is testing are]]
            [lox.scanner :as scanner]))

(defn- check-token-ok
  [input]
  (let [result (scanner/scan-tokens input)]
    (is (empty? (:errors result)))
    (butlast (:tokens result))))

(defn- check-token-lex-ok
  [input]
  (let [result (scanner/scan-tokens input)]
    (is (empty? (:errors result)))
    (butlast (map #(dissoc % :line :literal) (:tokens result)))))

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
           (count-tokens-errors "\"..."))))
  (testing "mixed tokens and errors"
    (is (= {:tokens [{:token :bang, :lexeme "!", :literal nil, :line 2}
                     {:token :eof, :lexeme nil, :literal nil, :line 2}]
            :errors [{:message "unexpected character $", :line 1}
                     {:message "unexpected character #", :line 1}
                     {:message "unexpected character ^", :line 2}
                     {:message "unexpected character &", :line 2}]}
           (scanner/scan-tokens "$#\n^&!")))))

(deftest test-single-char
  (are [input expected] (= expected (check-token-lex-ok input))
    "(" [{:token :left-paren :lexeme "("}]
    ")" [{:token :right-paren :lexeme ")"}]
    "{" [{:token :left-brace :lexeme "{"}]
    "}" [{:token :right-brace :lexeme "}"}]
    "," [{:token :comma :lexeme ","}]
    "." [{:token :dot :lexeme "."}]
    "-" [{:token :minus :lexeme "-"}]
    "+" [{:token :plus :lexeme "+"}]
    ";" [{:token :semicolon :lexeme ";"}]
    "*" [{:token :star :lexeme "*"}]))

(deftest test-equals-op
  (are [input expected] (= expected (check-token-lex-ok input))
    "!!" [{:token :bang :lexeme "!"}
          {:token :bang :lexeme "!"}]
    "!=" [{:token :bang-equal :lexeme "!="}]
    "=." [{:token :equal :lexeme "="}
          {:token :dot :lexeme "."}]
    "==" [{:token :equal-equal :lexeme "=="}]
    ">>" [{:token :greater :lexeme ">"}
          {:token :greater :lexeme ">"}]
    ">=" [{:token :greater-equal :lexeme ">="}]
    "<<" [{:token :less :lexeme "<"}
          {:token :less :lexeme "<"}]
    "<=" [{:token :less-equal :lexeme "<="}]))

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

(deftest test-reserved
  (are [input expected] (= expected (check-token-lex-ok input))
    "and" [{:token :and :lexeme "and"}]
    "class" [{:token :class :lexeme "class"}]
    "else" [{:token :else :lexeme "else"}]
    "false" [{:token :false :lexeme "false"}]
    "for" [{:token :for :lexeme "for"}]
    "fun" [{:token :fun :lexeme "fun"}]
    "if" [{:token :if :lexeme "if"}]
    "nil" [{:token :nil :lexeme "nil"}]
    "or" [{:token :or :lexeme "or"}]
    "print" [{:token :print :lexeme "print"}]
    "return" [{:token :return :lexeme "return"}]
    "super" [{:token :super :lexeme "super"}]
    "this" [{:token :this :lexeme "this"}]
    "true" [{:token :true :lexeme "true"}]
    "var" [{:token :var :lexeme "var"}]
    "while" [{:token :while :lexeme "while"}]))

(deftest test-identifier
  (are [input expected] (= expected (check-token-ok input))
    "forever" [{:token :identifier :lexeme "forever" :literal nil :line 1}]
    "meanwhile" [{:token :identifier :lexeme "meanwhile" :literal nil :line 1}]))

(comment
  (clojure.test/run-tests 'lox.scanner-test))
