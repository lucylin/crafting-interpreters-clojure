(ns lox.expr)

(defn ->literal-expr
  [value]
  {:type :literal-expr :value value})

(defn ->unary-expr
  [operator right]
  {:type :unary-expr :operator operator :right right})

(defn ->binary-expr
  [left operator right]
  {:type :binary-expr :left left :operator operator :right right})

(defn ->grouping-expr
  [expr]
  {:type :grouping-expr :expr expr})

(defn pretty-expr
  [expr]
  (case (:type expr)
    :literal-expr (:value expr)

    :unary-expr (str "("
                     ((:lexeme (:operator expr))
                      " "
                      (pretty-expr (:right expr)))
                     ")")

    :binary-expr (str "("
                      (:lexeme (:operator expr))
                      " "
                      (pretty-expr (:left expr))
                      " "
                      (pretty-expr (:right expr))
                      ")")

    :grouping-expr (pretty-expr (:expr expr))

    :else nil))
