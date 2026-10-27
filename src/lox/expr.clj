(ns lox.expr)

(defn ->literal-expr
  [value]
  {:type :literal-expr :value value})

(defn ->logical-expr
  [left operator right]
  {:type :logical-expr :left left :operator operator :right right})

(defn ->unary-expr
  [operator right]
  {:type :unary-expr :operator operator :right right})

(defn ->binary-expr
  [left operator right]
  {:type :binary-expr :left left :operator operator :right right})

(defn ->grouping-expr
  [expr]
  {:type :grouping-expr :expr expr})

(defn ->var-expr
  [var-name]
  {:type :var-expr :name var-name})

(defn ->assign-expr
  [var-name expr]
  {:type :assign-expr :name var-name :expr expr})

(defn pretty-expr
  [expr]
  (if
   (nil? expr) nil
   (case (:type expr)
     :literal-expr (:value expr)

     :logical-expr [(:type (:operator expr))
                    (pretty-expr (:left expr))
                    (pretty-expr (:right expr))]

     :unary-expr [(:type (:operator expr))
                  (pretty-expr (:right expr))]

     :binary-expr [(:type (:operator expr))
                   (pretty-expr (:left expr))
                   (pretty-expr (:right expr))]

     :grouping-expr [(pretty-expr (:expr expr))]

     :var-expr (:lexeme (:name expr))

     :assign-expr [(:lexeme (:name expr)) ":=" (pretty-expr (:expr expr))])))
