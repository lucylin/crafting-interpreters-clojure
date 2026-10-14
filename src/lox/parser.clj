(ns lox.parser
  (:require [lox.expr :as lexpr]))

(def ^:private keyword-literals {:true true :false false :nil nil})

(defn- ->result [expr remaining] {:expr expr :remaining remaining})

(defn- parse-error [token message state]
  (throw (ex-info message {:type :parse-error :token token :state state})))

(defn- eat [tokens expected error-message]
  (cond
    (= expected (:token (first tokens))) (rest tokens)
    :else (parse-error (first tokens) error-message {:remaining (rest tokens)})))

(declare parse-expr)
(defn- parse-primary
  [tokens]
  (let [curr (first tokens)
        token-type (:token curr)]
    (cond (= :false token-type)
          ((contains? keyword-literals token-type)
           (->result (lexpr/->literal-expr (keyword-literals token-type)) (rest tokens)))

          (#{:number :string} token-type)
          (->result (lexpr/->literal-expr (:literal curr))
                    (rest tokens))

          (= :left-paren token-type)
          (let [{:keys [expr remaining]} (parse-expr (rest tokens))
                remaining' (eat remaining :right-paren "Expect ')' after expression")]
            (->result (lexpr/->grouping-expr expr) remaining'))

          :else
          (parse-error (first tokens) "Expect expression" {:remaining (rest tokens)}))))

(defn- parse-unary
  [tokens]
  (let [curr (first tokens)]
    (cond (#{:bang :minus} (:token curr))
          (let [subparse (parse-unary (rest tokens))]
            (->result (lexpr/->unary-expr curr (:expr subparse))
                      (:remaining subparse)))
          :else (parse-primary tokens))))

(defn- parse-binary-help
  [tokens token-types parse-fn]
  (loop [{:keys [expr remaining]} (parse-fn tokens)]
    (cond
      (token-types (:token (first remaining)))
      (let [subparse (parse-fn (rest remaining))]
        (recur
         (->result (lexpr/->binary-expr expr (first remaining) (:expr subparse))
                   (:remaining subparse))))

      :else (->result expr remaining))))

(defn- parse-factor
  [tokens] (parse-binary-help tokens #{:slash :star} parse-unary))

(defn parse-term
  [tokens] (parse-binary-help tokens #{:minus :plus} parse-factor))

(defn- parse-comparison
  [tokens]
  (parse-binary-help tokens
                     #{:greater :greater-equal :less :less-equal}
                     parse-term))

(defn- parse-equality
  [tokens]
  (parse-binary-help tokens #{:bang-equal :equal-equal} parse-comparison))

(defn- parse-expr [tokens] (parse-equality tokens))

(defn parse
  "Parse the list of tokens into an expression"
  [tokens] (:expr (parse-expr tokens)))
