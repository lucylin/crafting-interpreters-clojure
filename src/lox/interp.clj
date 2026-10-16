(ns lox.interp)

(defn- runtime-error [op message state]
  (throw (ex-info message {:type :runtime-error :operation op :state state})))

(declare evaluate)
(defn- evaluate-unary
  [expr]
  (let [result (evaluate (:right expr))]
    (case (:token (:operator expr))
      :bang (not result)
      :minus (if (number? result) (- result)
                 (runtime-error (:operator expr)
                                "Expect number as argument"
                                {:right (:right expr)})))))

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
    ((binary-number-ops (:token op)) left right)

    :else (runtime-error op "Expect numbers as arguments"
                         {:left left :right right})))

(def ^:private equality-ops
  {:equal-equal =
   :bang-equal not=})

(defn- evaluate-binary
  [expr]
  (let [op (:operator expr)
        left (evaluate (:left expr))
        right (evaluate (:right expr))]

    (cond
      (= :plus (:token op))
      (cond
        (and (number? left) (number? right)) (+ left right)
        (and (string? left) (string? right)) (str left right)
        :else (runtime-error op "Expect numbers or strings as arguments"
                             {:left left :right right}))

      (contains? binary-number-ops (:token op))
      (evaluate-number-op left op right)

      (contains? equality-ops (:token op))
      ((equality-ops (:token op)) left right)

      :else (runtime-error op "Invalid binary operation"
                           {:left left :right right}))))

(defn- evaluate
  [expr]
  (case (:type expr)
    :literal-expr (:value expr)
    :grouping-expr (evaluate (:expr expr))
    :unary-expr (evaluate-unary expr)
    :binary-expr (evaluate-binary expr)
    (runtime-error nil "Unknown expression type" {:expr expr})))

(defn interpret
  [expr]
  (evaluate expr))
