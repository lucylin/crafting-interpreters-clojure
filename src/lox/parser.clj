(ns lox.parser
  (:require [lox.expr :as lexpr]))

(defn- ->result [expr remaining] {:expr expr :remaining remaining})

(defn- parse-binary
  [tokens token-types parse-fn]
  (loop [{:keys [expr remaining]} (parse-fn tokens)]
    (cond
      (token-types (:token (first remaining)))
      (let [subparse (parse-fn (rest remaining))]
        (recur (->result (lexpr/->binary-expr expr (first remaining) (:expr subparse))
                         (:remaining subparse))))

      :else (->result expr remaining))))

(defn- parse-primary
  [tokens]
  (let [curr (first tokens)
        token-type (:token curr)]
    ; TODO: handle :left-paren/:right-paren as grouping expr
    (cond (#{:false :true :nil :number :string} token-type)
          (->result (lexpr/->literal-expr (:literal curr))
                    (rest tokens)))))

(defn- parse-comparison
  [tokens]
  (parse-binary tokens #{:greater :greater-equal :less :less-equal}
                parse-primary))

(defn- parse-equality
  [tokens]
  (parse-binary tokens #{:bang-equal :equal-equal} parse-comparison))

(defn- parse-expr
  [tokens]
  (parse-equality tokens))

(defn parse
  [tokens]
  (:expr (parse-expr tokens)))

(comment
  (parse [{:token :number :literal 1}
          {:token :bang-equal}
          {:token :number :literal 2}
          {:token :bang-equal}
          {:token :number :literal 3}])
  (parse [{:token :number :literal 1}
          {:token :less-equal}
          {:token :number :literal 2}
          {:token :equal-equal}
          {:token :number :literal 2}]))
