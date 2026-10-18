(ns lox.parser
  (:require [lox.expr :as lexpr]
            [lox.stmt :as lstmt]
            [lox.scanner :as scanner]))

(defn- ->expr-result [expr remaining] {:expr expr :remaining remaining})

(defn- ->stmt-result
  ([stmt remaining] {:stmt stmt :remaining remaining :error nil})
  ([stmt remaining error] {:stmt stmt :remaining remaining :error error}))

(defn- parse-ex [token message state]
  (throw (ex-info message {:type :parse-error :token token :state state})))

(defn- parse-error [ex]
  (let [token (:token (ex-data ex))
        message (ex-message ex)]
    {:message message :line (:line token) :token (:token token)}))

(defn- eat [tokens expected error-message]
  (cond
    (= expected (:token (first tokens))) (rest tokens)
    :else (parse-ex (first tokens) error-message {:remaining (rest tokens)})))

;
; expr parsing
;

(def ^:private keyword-literals {:true true :false false :nil nil})

(declare parse-expr)
(defn- parse-primary
  [tokens]
  (let [curr (first tokens)
        token-type (:token curr)]
    (cond (contains? keyword-literals token-type)
          (->expr-result (lexpr/->literal-expr (keyword-literals token-type))
                         (rest tokens))

          (#{:number :string} token-type)
          (->expr-result (lexpr/->literal-expr (:literal curr))
                         (rest tokens))

          (= :left-paren token-type)
          (let [{:keys [expr remaining]} (parse-expr (rest tokens))
                remaining' (eat remaining :right-paren "Expect ')' after expression")]
            (->expr-result (lexpr/->grouping-expr expr) remaining'))

          :else
          (parse-ex (first tokens) "Expect expression"
                    {:remaining (rest tokens)}))))

(defn- parse-unary
  [tokens]
  (let [curr (first tokens)]
    (cond (#{:bang :minus} (:token curr))
          (let [subparse (parse-unary (rest tokens))]
            (->expr-result (lexpr/->unary-expr curr (:expr subparse))
                           (:remaining subparse)))
          :else (parse-primary tokens))))

(defn- parse-binary-help
  [tokens token-types parse-fn]
  (loop [{:keys [expr remaining]} (parse-fn tokens)]
    (cond
      (token-types (:token (first remaining)))
      (let [subparse (parse-fn (rest remaining))]
        (recur
         (->expr-result
          (lexpr/->binary-expr expr (first remaining) (:expr subparse))
          (:remaining subparse))))

      :else (->expr-result expr remaining))))

(defn- parse-factor
  [tokens] (parse-binary-help tokens #{:slash :star} parse-unary))

(defn- parse-term
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

;
; stmt parsing
;

(defn- parse-expr-stmt
  [tokens]
  (let [{:keys [expr remaining]} (parse-expr tokens)]
    (->stmt-result (lstmt/->expr-stmt expr) remaining)))

(defn- parse-print-stmt
  [tokens]
  (let [{:keys [expr remaining]} (parse-expr tokens)]
    (->stmt-result (lstmt/->print-stmt expr) remaining)))

(defn- parse-stmt
  [tokens]
  (let [curr (first tokens)
        {:keys [stmt remaining]} (case (:token curr)
                                   :print (parse-print-stmt (rest tokens))
                                   (parse-expr-stmt tokens))
        remaining' (eat remaining :semicolon
                        "Expect semicolon at end of statement")]
    (->stmt-result stmt remaining')))

;
; synchronize after errors
;

(def ^:private sync-tokens #{:semicolon :eof})

(defn- synchronize
  [tokens]
  (let [tokens' (drop-while #(not (contains? sync-tokens (:token %1))) tokens)]
    (cond
      (and (seq tokens') (= :semicolon (:token (first tokens'))))
      (eat tokens' :semicolon "Unreachable error (expect semicolon)")

      :else tokens')))

;
; public parse() fn
;

(defn parse
  "Parse the list of tokens into statements"
  [tokens]
  (loop [stmts []
         tokens' tokens
         errors []]
    (let [{:keys [stmt remaining error]}
          (try
            (parse-stmt tokens')
            (catch clojure.lang.ExceptionInfo e
              (let [state (ex-data e)]
                (->stmt-result nil
                               (synchronize (:remaining state))
                               (parse-error e)))))
          stmts' (if stmt (conj stmts stmt) stmts)
          errors' (if error (conj errors error) errors)]
      (cond
        (= :eof (:token (first remaining))) {:stmts stmts' :errors errors'}
        :else (recur stmts' remaining errors')))))

(comment
  (synchronize
   (synchronize (:tokens (scanner/scan-tokens "(foobar); (barfoo)")))))
