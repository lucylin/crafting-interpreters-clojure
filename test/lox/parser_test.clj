(ns lox.parser-test
  (:require [clojure.test :refer [deftest is testing are]]
            [lox.expr :as lexpr]
            [lox.stmt :as lstmt]
            [lox.parser :as parser]
            [lox.test-util :as util]))

(defn- pretty-parse-expr
  [code]
  (let [stmts (util/parse-ok code)
        stmt (first stmts)
        expr (:expr stmt)]
    (is (= :expr-stmt (:type stmt)))
    (lexpr/pretty-expr expr)))

(defn- parse-error-token
  [code]
  (let [{:keys [stmts errors]} (util/parse-str code)]
    (is (empty? stmts))
    (:token (first errors))))

(deftest test-expr
  (testing "empty program"
    (is (= [] (util/parse-ok ""))))

  (testing "literals"
    (are [input expected] (= expected (pretty-parse-expr input))
      "true;" true
      "false;" false
      "nil;" nil
      "\"hello\";" "hello"
      "123.45;" 123.45))

  (testing "associativity"
    (are [input expected] (= expected (pretty-parse-expr input))
      "1 / 2 / 3;" [:slash [:slash 1.0 2.0] 3.0]))

  (testing "equality-comparison precedence"
    (are [input expected] (= expected (pretty-parse-expr input))
      "1 < 2 != 2 > 1;" [:bang-equal [:less 1.0 2.0] [:greater 2.0 1.0]]))

  (testing "comparison-term precedence"
    (are [input expected] (= expected (pretty-parse-expr input))
      "1 + 3 >= 3;" [:greater-equal [:plus 1.0 3.0] 3.0]
      "1 + 3 <= 3;" [:less-equal [:plus 1.0 3.0] 3.0]))

  (testing "term-factor precedence"
    (are [input expected] (= expected (pretty-parse-expr input))
      "1 - 2 * 3;" [:minus 1.0 [:star 2.0 3.0]]
      "(1 + 2) * 3 == 6;" [:equal-equal [:star [[:plus 1.0 2.0]] 3.0] 6.0]))

  (testing "factor-unary precedence & other unary"
    (are [input expected] (= expected (pretty-parse-expr input))
      "-1 / 2;" [:slash [:minus 1.0] 2.0]
      "!!false;" [:bang [:bang false]]
      "1 - -2;" [:minus 1.0 [:minus 2.0]]))

  (testing "expr errors"
    (are [input expected] (= expected (parse-error-token input))
      ");" :right-paren
      "(1 + 2;" :semicolon
      "1 + 2);" :right-paren
      "1 + 2" :eof
      "1 +" :eof))

  (testing "var expr"
    (are [input expected] (= expected (pretty-parse-expr input))
      "x" ["x"]
      "x + 5" [:plus "x" 5.0])))

(deftest test-print
  (testing "print success"
    (are [input expected]
         (= expected (map lstmt/pretty-stmt (util/parse-ok input)))
      "print 5;" [[:print-stmt 5.0]]
      "print 1 + 2;" [[:print-stmt [:plus 1.0 2.0]]]
      "1; print 2;" [[:expr-stmt 1.0] [:print-stmt 2.0]]))

  (testing "print failure"
    (are [input expected] (= expected (parse-error-token input))
      "print;" :semicolon
      "print 1 print 2;" :print)

    (let [{:keys [stmts errors]}
          (util/parse-str "print 1 print 2; print 3; 1 +; 4;")]
      (is (= :print (:token (first errors))))
      (is (= [:print-stmt 3.0] (lstmt/pretty-stmt (first stmts))))
      (is (= :semicolon (:token (second errors))))
      (is (= [:expr-stmt 4.0] (lstmt/pretty-stmt (second stmts)))))))

(deftest test-var
  (testing "var declaration"
    (are [input expected]
         (= expected (map lstmt/pretty-stmt (util/parse-ok input)))
      "var x;" [:var-stmt :name "x" nil])))

(comment
  (clojure.test/run-tests 'lox.parser-test)
  (parser/parse (util/scan-ok "var x;")))
; [{:type :var, :lexeme "var", :literal nil, :line 1}
;  {:type :identifier, :lexeme "x", :literal nil, :line 1}
;  {:type :semicolon, :lexeme ";", :literal nil, :line 1}
;  {:type :eof, :lexeme nil, :literal nil, :line 1}]
