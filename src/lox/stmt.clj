(ns lox.stmt
  (:require [lox.expr :as lexpr]))

(defn ->print-stmt
  [expr]
  {:type :print-stmt :expr expr})

(defn ->expr-stmt
  [expr]
  {:type :expr-stmt :expr expr})

(defn ->var-stmt
  [var-name expr]
  {:type :var-stmt :name var-name :expr expr})

(defn pretty-stmt
  [stmt]
  (case (:type stmt)
    :print-stmt [:print-stmt (lexpr/pretty-expr (:expr stmt))]
    :expr-stmt [:expr-stmt (lexpr/pretty-expr (:expr stmt))]
    :var-stmt [:var-stmt :name (:name stmt) (lexpr/pretty-expr (:expr stmt))]))
