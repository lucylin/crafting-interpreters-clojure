(ns lox.interp
  (:require [lox.env :as lenv]
            [lox.error :as error]))

;
; evaluating exprs
;

(defn- ->eval-result [result env] [result env])

(declare evaluate)
(defn- evaluate-unary
  [expr env]
  (let [[expr-result env'] (evaluate (:right expr) env)
        result (case (:type (:operator expr))
                 :bang (not expr-result)
                 :minus (if (number? expr-result) (- expr-result)
                            (error/runtime-ex (:operator expr)
                                              "Expect number as argument")))]
    (->eval-result result env')))

(def ^:private binary-number-ops  ; except +, which is special
  {:minus -
   :star *
   :slash /
   :less <
   :less-equal <=
   :greater >
   :greater-equal >=})

(defn- evaluate-number-op
  [left op right]
  (cond
    (and (number? left) (number? right))
    ((binary-number-ops (:type op)) left right)

    :else (error/runtime-ex op "Expect numbers as arguments")))

(def ^:private equality-ops
  {:equal-equal =
   :bang-equal not=})

(defn- evaluate-binary
  [expr env]
  (let [op (:operator expr)
        [left env'] (evaluate (:left expr) env)
        [right env'] (evaluate (:right expr) env')
        result (cond
                 (= :plus (:type op))
                 (cond
                   (and (number? left) (number? right)) (+ left right)
                   (and (string? left) (string? right)) (str left right)
                   :else (error/runtime-ex
                          op "Expect numbers or strings as arguments"))

                 (contains? binary-number-ops (:type op))
                 (evaluate-number-op left op right)

                 (contains? equality-ops (:type op))
                 ((equality-ops (:type op)) left right)

                 :else (error/runtime-ex op "Invalid binary operation"))]
    (->eval-result result env')))

(defn- evaluate-assign
  [expr env]
  (let [var-name (:name expr)
        [new-val env'] (evaluate (:expr expr) env)]
    (->eval-result new-val (lenv/assign-var env' var-name new-val))))

(defn evaluate
  [expr env]
  (case (:type expr)
    :literal-expr (->eval-result (:value expr) env)
    :grouping-expr (evaluate (:expr expr) env)
    :unary-expr (evaluate-unary expr env)
    :binary-expr (evaluate-binary expr env)
    :var-expr (->eval-result (lenv/get-var env (:name expr)) env)
    :assign-expr (evaluate-assign expr env)
    (error/runtime-ex nil (str "Unknown expression type " (:type expr)))))

;
; interpreting stmts
;

(defn- interpret-stmt
  [stmt env]
  (case (:type stmt)
    :expr-stmt (second (evaluate (:expr stmt) env))

    :if-stmt
    (let [[cond-result env'] (evaluate (:cond-expr stmt) env)
          env' (if cond-result (interpret-stmt (:then-stmt stmt) env')
                   (if (:else-stmt stmt)
                     (interpret-stmt (:else-stmt stmt) env')
                     env'))]
      env')

    :print-stmt
    (let [[result env'] (evaluate (:expr stmt) env)]
      (println result)
      env')

    :var-stmt
    (let [var-name (:name stmt)
          [result env'] (if (:expr stmt) (evaluate (:expr stmt) env)
                            (->eval-result nil env))]
      (lenv/declare-var env' var-name result))

    :block-stmt
    (loop [env' (lenv/->lox-env env)
           inner-stmts (:stmts stmt)]
      (if (empty? inner-stmts) (:parent env')
          (recur (interpret-stmt (first inner-stmts) env') (rest inner-stmts))))

    (error/runtime-ex nil (str "Unknown statement type " (:type stmt)))))

;
; public interpret()
;

(defn interpret
  ([stmts] (interpret stmts (lenv/->lox-env)))

  ([stmts env]
   (loop [stmts' stmts
          env' env]
     (cond
       (empty? stmts') env'
       :else (recur (rest stmts') (interpret-stmt (first stmts') env'))))))
