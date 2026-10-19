(ns lox.test-util
  (:require [clojure.test :refer [is]]
            [lox.interp :as interp]
            [lox.parser :as parser]
            [lox.scanner :as scanner]))

(defn scan-ok
  [code]
  (let [{:keys [tokens errors]} (scanner/scan-tokens code)]
    (is (empty? errors))
    tokens))

(defn parse-str [code] (parser/parse (scan-ok code)))

(defn parse-ok
  [code]
  (let [{:keys [stmts errors]} (parse-str code)]
    (is (empty? errors))
    stmts))

(defn eval-expr-str [code] (interp/evaluate (:expr (first (parse-ok code)))))

; TODO:interp helpers
; (defn interp-str [code] (interp/interpret (parse-ok code)))

