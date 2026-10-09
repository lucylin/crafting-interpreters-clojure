(ns lox.scanner)

(def ^:private digits
  (set "0123456789"))

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

(defn- scan-num-lit
  "Scan a numeric literal"
  [state]
  (loop [literal (str (first (:remaining state)))
         c-seq (rest (:remaining state))]
    (cond
      (contains? digits (first c-seq))
      (recur (str literal (first c-seq)) (rest c-seq))

      (and (= (first c-seq) \.) (contains? digits (second c-seq)))
      (recur (str literal (first c-seq)) (rest c-seq))

      :else
      (-> state
          (assoc :remaining c-seq)
          (add-token :number literal (parse-double literal))))))

(defn- scan-str-lit
  "Scan a string literal"
  [state]
  (loop [literal ""
         new-lines 0
         c-curr (second (:remaining state))  ; ignore the starting "
         c-nexts (nthrest (:remaining state) 2)]
    (cond
      (nil? c-curr)
      (-> state
          (assoc :remaining c-nexts)
          (update :line + new-lines)
          (add-error "unterminated string"))

      (= c-curr \")
      (-> state
          (assoc :remaining c-nexts)
          (update :line + new-lines)
          (add-token :string (str \" literal \") literal))

      (= c-curr \newline)
      (recur (str literal c-curr) (inc new-lines) (first c-nexts) (rest c-nexts))

      :else
      (recur (str literal c-curr) new-lines (first c-nexts) (rest c-nexts)))))

(defn- scan-token
  "Scan the next token (comprising one or more characters)"
  [state]
  (let [c-curr (first (:remaining state))
        c-next (second (:remaining state))
        c-token (char->token c-curr)
        [op-token-bare op-token-eq] (op-eq->token c-curr)]
    (cond
      (= c-curr \newline)
      (-> state
          (update :remaining rest)
          (update :line inc))

      (contains? whitespace c-curr) (update state :remaining rest)

      (some? c-token)  ; ( ) + ...
      (-> state
          (update :remaining rest)
          (add-token c-token (str c-curr) nil))

      (some? op-token-bare)  ; ! != ...
      (let [c-next-eq (= c-next \=)
            tok (if c-next-eq op-token-eq op-token-bare)
            lex (if c-next-eq (str c-curr \=) (str c-curr))
            eat (if c-next-eq 2 1)]
        (-> state
            (update :remaining nthrest eat)
            (add-token tok lex nil)))

      (and (= c-curr \/) (= c-next \/))  ; // comments
      (let [eat (count (take-while (partial not= \newline) (:remaining state)))]
        (update state :remaining nthrest eat))

      (= c-curr \/)  ; regular /
      (-> state
          (update :remaining rest)
          (add-token :slash "/" nil))

      (= c-curr \") (scan-str-lit state)

      (contains? digits c-curr) (scan-num-lit state)

      :else
      (-> state
          (update :remaining rest)
          (add-error (str "unexpected character " c-curr))))))

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
  (scan-tokens "123\n456.78+\n9.!")
  (scan-tokens "\"lol\"\nlol"))
