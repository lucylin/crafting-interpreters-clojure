(ns lox.parser-test
  (:require [clojure.test :refer [deftest is testing are]]
            [lox.expr :as lexpr]
            [lox.stmt :as lstmt]
            [lox.parser :as parser]
            [lox.scanner :as scanner]))

(defn- pretty-parse-expr
  [code]
  (let [stmt
        (first (:stmts (parser/parse
                        (:tokens (scanner/scan-tokens (str code ";"))))))]
    (is (= :expr-stmt (:type stmt)))
    (lexpr/pretty-expr (:expr stmt))))

(defn- parse-error-data
  [code]
  (try (pretty-parse-expr code)
       :no-error
       (catch clojure.lang.ExceptionInfo e  ; TODO: we now catch exceptions in parse()
         (if (= :parse-error (:type (ex-data e)))
           (ex-data e)
           (throw e)))))

(deftest test-expr
  (testing "literals"
    (are [input expected] (= expected (pretty-parse-expr input))
      "true" true
      "false" false
      "nil" nil
      "\"hello\"" "hello"
      "123.45" 123.45))

  (testing "associativity"
    (are [input expected] (= expected (pretty-parse-expr input))
      "1 / 2 / 3" [:slash [:slash 1.0 2.0] 3.0]))

  (testing "equality-comparison precedence"
    (are [input expected] (= expected (pretty-parse-expr input))
      "1 < 2 != 2 > 1" [:bang-equal [:less 1.0 2.0] [:greater 2.0 1.0]]))

  (testing "comparison-term precedence"
    (are [input expected] (= expected (pretty-parse-expr input))
      "1 + 3 >= 3" [:greater-equal [:plus 1.0 3.0] 3.0]
      "1 + 3 <= 3" [:less-equal [:plus 1.0 3.0] 3.0]))

  (testing "term-factor precedence"
    (are [input expected] (= expected (pretty-parse-expr input))
      "1 - 2 * 3" [:minus 1.0 [:star 2.0 3.0]]
      "(1 + 2) * 3 == 6" [:equal-equal [:star [[:plus 1.0 2.0]] 3.0] 6.0]))

  (testing "factor-unary precedence & other unary"
    (are [input expected] (= expected (pretty-parse-expr input))
      "-1 / 2" [:slash [:minus 1.0] 2.0]
      "!!false" [:bang [:bang false]]
      "1 - -2" [:minus 1.0 [:minus 2.0]])))

; TODO: re-add these
;
;  (testing "expect expr"
;    (let [err (parse-error-data ")")]
;      (is (= :right-paren (:token (:token err)))))
;
;    (let [err (parse-error-data "1 +")]
;      (is (= :eof (:token (:token err))))))
;
;  (testing "incomplete expr"
;    (let [err (parse-error-data "(1 + 2")]
;      (is (= :eof (:token (:token err))))))
;
;  (testing "unexpected extra tokens"
;    (let [err (parse-error-data "1 + 2)")]
;      (is (= :right-paren (:token (:token err)))))))

(comment
  (clojure.test/run-tests 'lox.parser-test)
  (parser/parse (:tokens (scanner/scan-tokens "true;")))
  (pretty-parse-expr "true"))
