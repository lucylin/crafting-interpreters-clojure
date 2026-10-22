(ns lox.interp-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing are]]
            [lox.interp :as interp]
            [lox.test-util :as util]))

(defn- interp-code-print
  [code]
  (let [stmts (util/parse-ok code)]
    (str/split (with-out-str (interp/interpret stmts)) #"\n")))

(deftest test-interp-exprs
  (testing "literals"
    (are [input expected] (= expected (util/eval-expr-str input))
      "1;" 1.0))

  (testing "unary"
    (are [input expected] (= expected (util/eval-expr-str input))
      "-1;" -1.0
      "!!false;" false
      "!\"\";" false
      "!nil;" true))

  (testing "grouping & binary"
    (are [input expected] (= expected (util/eval-expr-str input))
      "3 * (5 - 1);" 12.0
      "3 + 2 < 3 + 3;" true
      "\"hello\" + \" \" + \"world\";" "hello world"
      "5 * 5 == (50 / 2);" true
      "\"foobar\" == \"foo\" + \"bar\";" true))

  (testing "logical"
    (are [input expected] (= expected (util/eval-expr-str input))
      ; TODO: test that shortcuts actually work
      "true and true;" true
      "true or false;" true
      "false and true;" false
      "(1 == 1) and (1 == 0);" false)))

(deftest test-interp-errors
  (testing "type handling"
    (are [input] (thrown? clojure.lang.ExceptionInfo (util/eval-expr-str input))
      "-\"a\";"
      "1 < \"foo\";"
      "\"x\" - \"y\";"
      "false / true;"
      "nil + nil;"
      "1 + \"a\";")))

(deftest test-interp-print
  ; TODO: test that print flushes to output before a runtime error

  (testing "print expr"
    (are [input expected] (= expected (interp-code-print input))
      "print 5;" ["5.0"]
      "print (2 * (5 + 3));" ["16.0"]
      "" [""])))

(deftest test-interp-var
  ; TODO: test error cases

  (testing "basics"
    (are [input expected] (= expected (interp-code-print input))
      "var x = 5; print x;" ["5.0"]
      "var x = 5; x = x + 7; print x;" ["12.0"]))

  (testing "blocks"
    (are [input expected] (= expected (interp-code-print input))
      "{ var global = 5; print global; }" ["5.0"]
      "var global = 5; { var local = global + 7; print local; }" ["12.0"]
      "var global = 5; { global = global * 2; } print global;" ["10.0"]
      "var shadowed = 5; { var shadowed = shadowed * 2; } print shadowed;" ["5.0"]
      "var global = 5; { { var local = 42; global = local; print global; } }" ["42.0"])))

(deftest test-interp-ctrl
  (testing "if"
    (are [input expected] (= expected (interp-code-print input))
      "if (1 == 1) { print true; } else { print false; }" ["true"]
      "var global = 5; if (global == 2) { print true; } else { print false; }" ["false"]))

  (testing "while"
    (are [input expected] (= expected (interp-code-print input))
      "while (false) { print \"oops\"; } print false;" ["false"]
      "var x = 2; while (x > 0) { print x; x = x - 1; } print x;" ["2.0" "1.0" "0.0"])))

(comment
  (clojure.test/run-tests 'lox.interp-test))
