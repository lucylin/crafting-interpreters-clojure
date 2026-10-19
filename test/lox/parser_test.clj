(ns lox.parser-test
  (:require [clojure.test :refer [deftest is testing are]]
            [lox.expr :as lexpr]
            [lox.stmt :as lstmt]
            [lox.parser :as parser]
            [lox.scanner :as scanner]))

(defn- parse-code-str
  [code]
  (parser/parse (:tokens (scanner/scan-tokens code))))

(defn- parse-code-ok
  [code]
  (let [{:keys [stmts errors]} (parse-code-str code)]
    (is (empty? errors))
    stmts))

(defn- pretty-parse-expr
  [code]
  (let [stmts (parse-code-ok code)
        stmt (first stmts)
        expr (:expr stmt)]
    (is (= :expr-stmt (:type stmt)))
    (lexpr/pretty-expr expr)))

(defn- parse-error-token
  [code]
  (let [{:keys [stmts errors]} (parse-code-str code)]
    (is (empty? stmts))
    (:token (first errors))))

(deftest test-expr
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
      "" :eof)))

(deftest test-print
  (testing "print success"
    (are [input expected]
         (= expected (lstmt/pretty-stmt (first (parse-code-ok input))))
      "print 5;" [:print-stmt 5.0]
      "print 1 + 2;" [:print-stmt [:plus 1.0 2.0]]))

  (testing "print failure"
    (are [input expected] (= expected (parse-error-token input))
      "print;" :semicolon
      "print 1 print 2;" :print)

    (let [{:keys [stmts errors]} (parse-code-str "print 1 print 2; print 3;")]
      (is (= [:print-stmt 3.0] (lstmt/pretty-stmt (first stmts))))
      (is (= :print (:token (first errors)))))))

(comment
  (clojure.test/run-tests 'lox.parser-test))
