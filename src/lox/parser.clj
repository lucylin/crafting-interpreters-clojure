(ns lox.parser
  (:require [lox.expr :as lexpr]
            [lox.stmt :as lstmt]
            [lox.scanner :as scanner]))

(defn- ->expr-result [expr remaining] {:expr expr :remaining remaining})

(defn- ->stmt-result
  ([stmt remaining] (->stmt-result stmt remaining nil))
  ([stmt remaining error] {:stmt stmt :remaining remaining :error error}))

(defn- parse-ex [token message state]
  (throw (ex-info message {:type :parse-error :token token :state state})))

(defn- parse-error [ex]
  (let [token (:token (ex-data ex))
        message (ex-message ex)]
    {:type :parse-error :message message
     :line (:line token) :token (:type token)}))

(defn- eat [tokens expected error-message]
  (cond
    (= expected (:type (first tokens)))
    {:eaten (first tokens) :remaining (rest tokens)}

    :else (parse-ex (first tokens) error-message {:remaining tokens})))

;
; expr parsing
;

(def ^:private keyword-literals {:true true :false false :nil nil})

(declare parse-expr)
(defn- parse-primary
  [tokens]
  (let [curr (first tokens)
        token-type (:type curr)]
    (cond (contains? keyword-literals token-type)
          (->expr-result (lexpr/->literal-expr (keyword-literals token-type))
                         (rest tokens))

          (#{:number :string} token-type)
          (->expr-result (lexpr/->literal-expr (:literal curr))
                         (rest tokens))

          (= :left-paren token-type)
          (let [{:keys [expr remaining]} (parse-expr (rest tokens))
                remaining' (:remaining (eat remaining
                                            :right-paren
                                            "Expect ')' after expression"))]
            (->expr-result (lexpr/->grouping-expr expr) remaining'))

          (= :identifier token-type)
          (->expr-result (lexpr/->var-expr curr) (rest tokens))

          :else
          (parse-ex (first tokens) "Expect expression"
                    {:remaining tokens}))))

(defn- parse-unary
  [tokens]
  (let [curr (first tokens)]
    (cond (#{:bang :minus} (:type curr))
          (let [subparse (parse-unary (rest tokens))]
            (->expr-result (lexpr/->unary-expr curr (:expr subparse))
                           (:remaining subparse)))
          :else (parse-primary tokens))))

(defn- parse-binary-help
  ([tokens token-types parse-fn]
   (parse-binary-help tokens token-types parse-fn lexpr/->binary-expr))

  ([tokens token-types parse-fn expr-type]
   (loop [{:keys [expr remaining]} (parse-fn tokens)]
     (cond
       (token-types (:type (first remaining)))
       (let [subparse (parse-fn (rest remaining))]
         (recur
          (->expr-result
           (expr-type expr (first remaining) (:expr subparse))
           (:remaining subparse))))

       :else (->expr-result expr remaining)))))

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

(defn- parse-and
  [tokens] (parse-binary-help tokens #{:and} parse-equality lexpr/->logical-expr))

(defn- parse-or
  [tokens] (parse-binary-help tokens #{:or} parse-and lexpr/->logical-expr))

(defn- parse-assign
  [tokens]
  (let [{:keys [expr remaining]} (parse-or tokens)]
    (if
     (= :equal (:type (first remaining)))
      (let [{rhs :expr, remaining' :remaining} (parse-assign (rest remaining))]
        (if (= :var-expr (:type expr))
          (->expr-result (lexpr/->assign-expr (:name expr) rhs) remaining')
          (parse-ex (first remaining)
                    "Invalid assignment target"
                    (rest remaining))))

      (->expr-result expr remaining))))

(defn- parse-expr [tokens] (parse-assign tokens))

;
; synchronize after errors
;

(def ^:private sync-keywords
  #{:class :for :fun :if :print :return :var :while})

(def ^:private sync-tokens #{:semicolon :eof})

(defn- is-sync-token
  [token]
  (or (contains? sync-tokens (:type token))
      (contains? sync-keywords (:type token))))

(defn- synchronize
  [tokens]
  (let [tokens' (if (contains? sync-tokens (:type (first tokens))) tokens
                    (drop-while #(not (is-sync-token %1)) (rest tokens)))]
    (cond
      (= :semicolon (:type (first tokens')))
      (:remaining
       (eat tokens' :semicolon "Unreachable error (expect semicolon)"))

      ; don't eat keywords
      :else tokens')))

;
; stmt parsing
;

(defn- parse-stmt-w-expr
  [stmt-fn tokens]
  (let [{:keys [expr remaining]} (parse-expr tokens)
        remaining' (:remaining
                    (eat remaining :semicolon
                         "Expect semicolon at end of statement"))]
    (->stmt-result (stmt-fn expr) remaining')))

(declare parse-declaration)
(defn- parse-block-stmt
  [tokens]
  (loop [stmts []
         tokens' tokens
         errors []]
    (if (#{:right-brace :eof} (:type (first tokens')))
      (let [remaining (:remaining (eat tokens'
                                       :right-brace
                                       "Expect right brace after block"))]
        (->stmt-result (lstmt/->block-stmt stmts) remaining))

      (let [{:keys [stmt remaining error]} (parse-declaration tokens')
            stmts' (if stmt (conj stmts stmt) stmts)
            errors' (if error (conj errors error) errors)]
        (recur stmts' remaining errors')))))

(declare parse-stmt)
(defn- parse-if-stmt
  [tokens]
  (let [start-cond (:remaining (eat tokens
                                    :left-paren
                                    "Expect left parenthesis after if"))
        {cond-expr :expr remaining :remaining} (parse-expr start-cond)
        end-cond (:remaining (eat remaining
                                  :right-paren
                                  "Expect right parenthesis after if condition"))
        {then-stmt :stmt remaining :remaining}
        (parse-stmt end-cond)

        {else-stmt :stmt remaining :remaining}
        (if (= :else (:type (first remaining)))
          (parse-stmt (rest remaining))
          (->stmt-result nil remaining))]

    (->stmt-result (lstmt/->if-stmt cond-expr then-stmt else-stmt) remaining)))

(defn- parse-while-stmt
  [tokens]
  ; TODO: dedup with if-stmt logic?
  (let [start-cond (:remaining (eat tokens
                                    :left-paren
                                    "Expect left parenthesis after while"))
        {cond-expr :expr remaining :remaining} (parse-expr start-cond)
        end-cond (:remaining (eat remaining
                                  :right-paren
                                  "Expect right parenthesis after condition"))
        {body-stmt :stmt remaining :remaining} (parse-stmt end-cond)]
    (->stmt-result (lstmt/->while-stmt cond-expr body-stmt) remaining)))

(defn- parse-stmt
  [tokens]
  (let [curr (first tokens)
        {:keys [stmt remaining]}
        (case (:type curr)
          :if (parse-if-stmt (rest tokens))
          :print (parse-stmt-w-expr lstmt/->print-stmt (rest tokens))
          :while (parse-while-stmt (rest tokens))
          :left-brace (parse-block-stmt (rest tokens))
          (parse-stmt-w-expr lstmt/->expr-stmt tokens))]
    (->stmt-result stmt remaining)))

(defn- parse-declaration
  [tokens]
  (try
    (if (= :var (:type (first tokens)))
      (let [{:keys [eaten remaining]} (eat (rest tokens)
                                           :identifier "Expect variable name")
            var-name eaten
            initializer (if (= :equal (:type (first remaining)))
                          (parse-expr (rest remaining))
                          nil)
            eat-semicolon (eat (if initializer (:remaining initializer) remaining)
                               :semicolon
                               "Expect ';' after variable declaration")
            remaining' (:remaining eat-semicolon)
            stmt (lstmt/->var-stmt var-name
                                   (if initializer (:expr initializer) nil))]
        (->stmt-result stmt remaining'))

      ; else: not a var declaration
      (parse-stmt tokens))

    (catch clojure.lang.ExceptionInfo e
      (let [state (:state (ex-data e))]
        (->stmt-result nil (synchronize (:remaining state)) (parse-error e))))))

;
; public parse() fn
;

(defn parse
  "Parse the list of tokens into statements"
  [tokens]
  (loop [stmts []
         tokens' tokens
         errors []]
    (cond
      (= :eof (:type (first tokens'))) {:stmts stmts :errors errors}

      :else
      (let [{:keys [stmt remaining error]} (parse-declaration tokens')
            stmts' (if stmt (conj stmts stmt) stmts)
            errors' (if error (conj errors error) errors)]
        (recur stmts' remaining errors')))))

(comment
  (synchronize
   (synchronize (:tokens (scanner/scan-tokens "(foobar); (barfoo)"))))
  (parse (:tokens (scanner/scan-tokens "if (1 == 1) {print true;} else {print false;} ")))
  (parse (:tokens (scanner/scan-tokens "while (1 == 1) { print true; }"))))
