(ns lox.parser-test
  (:require [clojure.test :refer [deftest is testing are]]
            [lox.expr :as lexpr]
            [lox.parser :as parser]
            [lox.scanner :as scanner]))

(defn- pretty-parse
  [code] (lexpr/pretty-expr (parser/parse (:tokens (scanner/scan-tokens code)))))

(deftest test-expr
  (are [input expected] (= expected (pretty-parse input))
    "(1 + 2 * 3)" "(+ 1.0 (* 2.0 3.0))"
    "(1 + 2) * 3 == 6 != 7" "(!= (== (* (+ 1.0 2.0) 3.0) 6.0) 7.0)"))

(comment
  (clojure.test/run-tests 'lox.parser-test))
