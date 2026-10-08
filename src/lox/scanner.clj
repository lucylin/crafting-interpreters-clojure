(ns lox.scanner)

(def ^:private char->token
  "Single-character map to token type" {
  \( :left-paren
  \) :right-paren
  \{ :left-brace
  \} :right-brace
  \, :comma
  \. :dot
  \- :minus
  \+ :plus
  \; :semicolon
  \* :star })


(def ^:private op-eq->token
  "One/two-character map to token type" {
  \! [:bang :bang_equal]
  \= [:equal :equal_equal]
  \> [:greater :greater_equal]
  \< [:less :less_equal] })


(defn- scan-token
  [state]
  (let [c (first (:remaining state))
        c-next (second (:remaining state))
        c-tok (char->token c)
        [op-tok-bare op-tok-eq] (op-eq->token c)]
    (cond
      (= c \newline)
        (-> state
          (update :remaining rest)
          (update :line inc))

      (= c \/)  ; TODO: handle "//" comments
       (-> state
        (update :remaining rest)
        (update :tokens conj
          {:type :slash :lexeme "/" :literal nil :line (:line state)}))

      (some? c-tok)
        (-> state
          (update :remaining rest)
          (update :tokens conj
            {:type c-tok :lexeme (str c) :literal nil :line (:line state)}))

      (some? op-tok-bare)
        (let [c-next-eq (= c-next \=)
              tok (if c-next-eq op-tok-eq op-tok-bare)
              lex (if c-next-eq (str c \=) (str c))
              eat (if c-next-eq 2 1)]
          (-> state
            (update :remaining (partial drop eat))
            (update :tokens conj
              {:type tok :lexeme lex :literal nil :line (:line state)})))

      :else {})))  ; TODO: more stuff i guess


(defn scan-tokens
  "Scan tokens from the given code string"
  [code]
  (loop [state {:remaining code :line 1 :tokens [] :errors []}]
    (cond
      (nil? (first (:remaining state)))
        {:tokens (conj (:tokens state) :eof) :errors (:errors state)}
      :else (recur (scan-token state)))))


(comment
  (scan-tokens "(){}")
  (scan-tokens "/(\n)")
  (scan-tokens "!\n!=")
  (scan-tokens "lol"))
