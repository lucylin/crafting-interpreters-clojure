(ns lox.interp)

(defn- runtime-ex
  [op message]
  (throw (ex-info message {:type :runtime-error :line (:line op) :token (:type op)})))

(declare evaluate)
(defn- evaluate-unary
  [expr]
  (let [result (evaluate (:right expr))]
    (case (:type (:operator expr))
      :bang (not result)
      :minus (if (number? result) (- result)
                 (runtime-ex (:operator expr)
                             "Expect number as argument")))))

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

    :else (runtime-ex op "Expect numbers as arguments")))

(def ^:private equality-ops
  {:equal-equal =
   :bang-equal not=})

(defn- evaluate-binary
  [expr]
  (let [op (:operator expr)
        left (evaluate (:left expr))
        right (evaluate (:right expr))]

    (cond
      (= :plus (:type op))
      (cond
        (and (number? left) (number? right)) (+ left right)
        (and (string? left) (string? right)) (str left right)
        :else (runtime-ex op "Expect numbers or strings as arguments"))

      (contains? binary-number-ops (:type op))
      (evaluate-number-op left op right)

      (contains? equality-ops (:type op))
      ((equality-ops (:type op)) left right)

      :else (runtime-ex op "Invalid binary operation"))))

(defn evaluate
  [expr]
  (case (:type expr)
    :literal-expr (:value expr)
    :grouping-expr (evaluate (:expr expr))
    :unary-expr (evaluate-unary expr)
    :binary-expr (evaluate-binary expr)
    (runtime-ex nil (str "Unknown expression type " (:type expr)))))

(defn- interpret-stmt
  [stmt]
  (case (:type stmt)
    :expr-stmt (evaluate (:expr stmt))
    :print-stmt (println (evaluate (:expr stmt)))
    (runtime-ex nil (str "Unknown statement type " (:type stmt)))))

(defn interpret
  [stmts]
  (loop [stmts' stmts]
    (cond
      (empty? stmts') nil
      :else (do
              (interpret-stmt (first stmts'))
              (recur (rest stmts'))))))
