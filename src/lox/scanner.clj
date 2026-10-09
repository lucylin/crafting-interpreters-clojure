(ns lox.scanner)


(def ^:private whitespace
  "Whitespace characters to discard" #{
  \space \return \tab })


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
  \! [:bang :bang-equal]
  \= [:equal :equal-equal]
  \> [:greater :greater-equal]
  \< [:less :less-equal] })


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

      (contains? whitespace c)
        (state update :remaining rest)

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
            (update :remaining nthrest eat)
            (update :tokens conj
              {:type tok :lexeme lex :literal nil :line (:line state)})))

      (and (= c \/) (= c-next \/))
        (let [eat (count (take-while (partial not= \newline) (:remaining state)))]
          (update state :remaining nthrest eat))

      (= c \/)
        (-> state
          (update :remaining rest)
          (update :tokens conj
            {:type :slash :lexeme "/" :literal nil :line (:line state)}))

      (= c \")
        (loop [literal ""
               added-lines 0
               c-curr c-next
               remaining (nthrest (:remaining state) 2)]
          (cond
            (nil? c-curr)
              (-> state
                (assoc :remaining remaining)
                (update :line (partial + added-lines))
                (update :error conj
                  {:message "unterminated string" :line (:line state)}))
            (= c-curr \")
              (-> state
                (assoc :remaining remaining) ; ignore final "
                (update :line (partial + added-lines))
                (update :tokens conj
                  {:type :string :lexeme nil :literal literal :line (:line state)}))
            (= c-curr \newline)
              (recur (str literal c-curr) (inc added-lines)
                     (first remaining) (rest remaining))
            :else
              (recur (str literal c-curr) added-lines
                     (first remaining) (rest remaining))))

      :else
        (-> state
          (update :remaining rest)
          (update :errors conj
            {:message (str "invalid char " c) :line (:line state)})))))


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
  (scan-tokens "//\n(\n)")
  (scan-tokens "!\n!=")
  (scan-tokens "\"lol\"\nlol"))
