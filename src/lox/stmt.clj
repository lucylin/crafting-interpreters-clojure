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
    :cond cond-expr :then then-stmt :else else-stmt}))

(defn ->while-stmt
  [cond-expr body]
  {:type :while-stmt :cond cond-expr :body body})

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
               (lexpr/pretty-expr (:cond stmt))
               (pretty-stmt (:then stmt))
               (pretty-stmt (:else stmt))]

     :while-stmt [:while-stmt
                  (lexpr/pretty-expr (:cond stmt))
                  (pretty-stmt (:body stmt))])))
