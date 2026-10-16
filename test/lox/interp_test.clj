(ns lox.interp-test
  (:require [clojure.test :refer [deftest is testing are]]
            [lox.interp :as interp]
            [lox.parser :as parser]
            [lox.scanner :as scanner]))

(defn- evaluate-code-str
  [code]
  (interp/interpret (parser/parse (:tokens (scanner/scan-tokens code)))))

(deftest test-interp
  (testing "literals"
    (are [input expected] (= expected (evaluate-code-str input))
      "1" 1.0))

  (testing "unary"
    (are [input expected] (= expected (evaluate-code-str input))
      "-1" -1.0
      "!!false" false
      "!\"\"" false
      "!nil" true))

  (testing "grouping & binary"
    (are [input expected] (= expected (evaluate-code-str input))
      "3 * (5 - 1)" 12.0
      "3 + 2 < 3 + 3" true
      "\"hello\" + \" \" + \"world\"" "hello world"
      "5 * 5 == (50 / 2)" true
      "\"foobar\" == \"foo\" + \"bar\"" true)))

(deftest test-interp-errors
  (testing "type handling"
    (are [input] (thrown? clojure.lang.ExceptionInfo (evaluate-code-str input))
      "-\"a\""
      "1 < \"foo\""
      "\"x\" - \"y\""
      "false / true"
      "nil + nil"
      "1 + \"a\"")))

(comment
  (clojure.test/run-tests 'lox.interp-test))
