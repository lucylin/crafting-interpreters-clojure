(ns lox.scanner)

(def ^:private whitespace
  "Whitespace characters to discard"
  #{\space \return \tab})

(def ^:private char->token
  "Single-character map to token type"
  {\( :left-paren
   \) :right-paren
   \{ :left-brace
   \} :right-brace
   \, :comma
   \. :dot
   \- :minus
   \+ :plus
   \; :semicolon
   \* :star})

(def ^:private op-eq->token
  "One/two-character map to token type"
  {\! [:bang :bang-equal]
   \= [:equal :equal-equal]
   \> [:greater :greater-equal]
   \< [:less :less-equal]})

(defn- add-error
  [state error]
  (update state :errors conj {:message error :line (:line state)}))

(defn- add-token
  [state token-type lexeme literal]
  (update state :tokens conj
          {:type token-type :lexeme lexeme :literal literal
           :line (:line state)}))

(defn- scan-str-lit
  "Scan a string literal"
  [state]
  (loop [literal ""
         new-lines 0
         c-curr (second (:remaining state))  ; ignore the starting "
         remaining (nthrest (:remaining state) 2)]
    (cond
      (nil? c-curr)
      (-> state
          (assoc :remaining remaining)
          (update :line + new-lines)
          (add-error "unterminated string"))

      (= c-curr \")
      (-> state
          (assoc :remaining remaining)
          (update :line + new-lines)
          (add-token :string (str \" literal \") literal))

      (= c-curr \newline)
      (recur (str literal c-curr) (inc new-lines)
             (first remaining) (rest remaining))

      :else
      (recur (str literal c-curr) new-lines
             (first remaining) (rest remaining)))))

(defn- scan-token
  "Scan the next token (comprising one or more characters)"
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

      (contains? whitespace c) (update state :remaining rest)

      (some? c-tok)
      (-> state
          (update :remaining rest)
          (add-token c-tok (str c) nil))

      (some? op-tok-bare)
      (let [c-next-eq (= c-next \=)
            tok (if c-next-eq op-tok-eq op-tok-bare)
            lex (if c-next-eq (str c \=) (str c))
            eat (if c-next-eq 2 1)]
        (-> state
            (update :remaining nthrest eat)
            (add-token tok lex nil)))

      (and (= c \/) (= c-next \/))
      (let [eat (count (take-while (partial not= \newline) (:remaining state)))]
        (update state :remaining nthrest eat))

      (= c \/)
      (-> state
          (update :remaining rest)
          (add-token :slash "/" nil))

      (= c \") (scan-str-lit state)

      :else
      (-> state
          (update :remaining rest)
          (add-error (str "unexpected character " c))))))

(defn scan-tokens
  "Scan tokens from the given code string"
  [code]
  (loop [state {:remaining code :line 1 :tokens [] :errors []}]
    (cond
      (nil? (first (:remaining state)))
      ; TODO: add a proper token for :eof
      {:tokens (conj (:tokens state) :eof) :errors (:errors state)}
      :else (recur (scan-token state)))))

(comment
  (scan-tokens "( ){ }")
  (scan-tokens "//hello\n(\n)")
  (scan-tokens "!\n!=")
  (scan-tokens "\"lol\"\nlol"))
