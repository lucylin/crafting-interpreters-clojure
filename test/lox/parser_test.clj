(ns lox.parser-test
  (:require [clojure.test :refer [deftest is testing are]]
            [lox.expr :as lexpr]
            [lox.parser :as parser]
            [lox.scanner :as scanner]))

(defn- pretty-parse
  [code] (lexpr/pretty-expr (parser/parse (:tokens (scanner/scan-tokens code)))))

(defn- parse-error-data
  [code]
  (try (pretty-parse code)
       (catch clojure.lang.ExceptionInfo e (ex-data e))))

(deftest test-expr
  (testing "associativity"
    (are [input expected] (= expected (pretty-parse input))
      "1 / 2 / 3" [:slash [:slash 1.0 2.0] 3.0]))

  (testing "precedence"
    (are [input expected] (= expected (pretty-parse input))
      "1 + 2 * 3" [:plus 1.0 [:star 2.0 3.0]]
      "(1 + 2) * 3 == 6" [:equal-equal [:star [[:plus 1.0 2.0]] 3.0] 6.0]))

  (testing "incomplete expr"
    (is (= :parse-error (:type (parse-error-data "(1 + 2"))))))

(comment
  (clojure.test/run-tests 'lox.parser-test))
