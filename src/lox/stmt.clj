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

(defn ->block-stmt
  [stmts]
  {:type :block-stmt :stmts stmts})

(defn ->if-stmt
  ([cond-expr then-stmt] (->if-stmt cond-expr then-stmt nil))

  ([cond-expr then-stmt else-stmt]
   {:type :if-stmt
    :cond-expr cond-expr :then-stmt then-stmt :else-stmt else-stmt}))

(defn pretty-stmt
  [stmt]
  (if
   (nil? stmt) nil
   (case (:type stmt)
     :print-stmt [:print-stmt (lexpr/pretty-expr (:expr stmt))]
     :expr-stmt [:expr-stmt (lexpr/pretty-expr (:expr stmt))]
     :var-stmt [:var-stmt
                :name (:lexeme (:name stmt))
                (lexpr/pretty-expr (:expr stmt))]

     :block-stmt [:block-stmt (map pretty-stmt (:stmts stmt))]

     :if-stmt [:if-stmt
               (lexpr/pretty-expr (:cond-expr stmt))
               (pretty-stmt (:then-stmt stmt))
               (pretty-stmt (:else-stmt stmt))])))
