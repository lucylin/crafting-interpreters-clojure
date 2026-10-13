(ns lox.scanner)

(def ^:private digits
  "Numeric digits"
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

(def ^:private char-eq->token
  "One/two-character map to token type"
  {\! [:bang :bang-equal]
   \= [:equal :equal-equal]
   \> [:greater :greater-equal]
   \< [:less :less-equal]})

(def ^:private str->token
  "String map to keyword type"
  {"and"    :and
   "class"  :class
   "else"   :else
   "false"  :false
   "for"    :for
   "fun"    :fun
   "if"     :if
   "nil"    :nil
   "or"     :or
   "print"  :print
   "return" :return
   "super"  :super
   "this"   :this
   "true"   :true
   "var"    :var
   "while"  :while})

(defn- alpha? [c]
  (or (<= (int \a) (int c) (int \z))
      (<= (int \A) (int c) (int \Z))
      (= \_ c)))

(defn- digit? [c] (contains? digits c))

(defn- alphanum? [c] (or (alpha? c) (digit? c)))

(defn- add-error
  [state error]
  (update state :errors conj {:message error :line (:line state)}))

(defn- add-token
  [state token-type lexeme literal]
  (update state :tokens conj
          {:token token-type :lexeme lexeme :literal literal
           :line (:line state)}))

(defn- split-num-frac
  "Split characters into dot-fraction & rest"
  [cs]
  (let [[dot-c post-dot-c] cs]
    (if (and (= dot-c \.) (digit? post-dot-c))
      (let [[frac-cs rest-cs] (split-with digits (rest cs))]
        [(cons \. frac-cs) rest-cs])
      [nil cs])))

(defn- scan-num-lit
  "Scan a numeric literal and return updated scan state"
  [state]
  (let [[int-cs post-int-cs] (split-with digits (:remaining state))
        [frac-cs rest-cs] (split-num-frac post-int-cs)
        lexeme (apply str (concat int-cs frac-cs))]
    (-> state
        (assoc :remaining rest-cs)
        (add-token :number lexeme (parse-double lexeme)))))

(defn- scan-str-lit
  "Scan a string literal and return updated scan state"
  [state]
  (loop [literal ""
         newlines 0
         c (second (:remaining state))  ; ignore the starting "
         cs (nthrest (:remaining state) 2)]
    (cond
      (nil? c)
      (-> state
          (assoc :remaining cs)
          (update :line + newlines)
          (add-error "unterminated string"))

      (= c \")
      (-> state
          (assoc :remaining cs)
          (update :line + newlines)
          (add-token :string (str \" literal \") literal))

      (= c \newline)
      (recur (str literal c) (inc newlines) (first cs) (rest cs))

      :else
      (recur (str literal c) newlines (first cs) (rest cs)))))

(defn- scan-identifier
  "Scan an identifier/keyword and return updated scan state"
  [state]
  (let [id-cs (take-while alphanum? (:remaining state))
        eat-n (count id-cs)
        lexeme (apply str id-cs)
        token (str->token lexeme :identifier)]
    (-> state
        (update :remaining nthrest eat-n)
        (add-token token lexeme nil))))

(defn- scan-token
  "Scan the next token (one or more characters) and return updated scan state"
  [state]
  (let [c (first (:remaining state))
        c-token (char->token c)
        next-c (second (:remaining state))
        [char-eq-token-bare char-eq-token-eq] (char-eq->token c)]
    (cond
      (= c \newline)
      (-> state
          (update :remaining rest)
          (update :line inc))

      (contains? whitespace c) (update state :remaining rest)

      (some? c-token)  ; ( ) + ...
      (-> state
          (update :remaining rest)
          (add-token c-token (str c) nil))

      (some? char-eq-token-bare)  ; ! != ...
      (let [c-next-eq (= next-c \=)
            token (if c-next-eq char-eq-token-eq char-eq-token-bare)
            lexeme (if c-next-eq (str c \=) (str c))
            eat-n (if c-next-eq 2 1)]
        (-> state
            (update :remaining nthrest eat-n)
            (add-token token lexeme nil)))

      (and (= c \/) (= next-c \/))  ; // comments
      (let [eat-n (count (take-while (partial not= \newline)
                                     (:remaining state)))]
        (update state :remaining nthrest eat-n))

      (= c \/)  ; regular /
      (-> state
          (update :remaining rest)
          (add-token :slash "/" nil))

      (= c \") (scan-str-lit state)
      (digit? c) (scan-num-lit state)
      (alpha? c) (scan-identifier state)

      :else
      (-> state
          (update :remaining rest)
          (add-error (str "unexpected character " c))))))

(defn scan-tokens
  "Scan tokens from the given code string and return tokens & errors"
  [code]
  (loop [state {:remaining code :line 1 :tokens [] :errors []}]
    (cond
      (nil? (first (:remaining state)))
      {:tokens (conj (:tokens state)
                     {:token :eof :lexeme nil :literal nil :line (:line state)})
       :errors (:errors state)}
      :else (recur (scan-token state)))))

(comment
  (scan-tokens "( ){ }")
  (scan-tokens "//hello\n(\n)")
  (scan-tokens "!\n!=")
  (scan-tokens "123\n456.78+\n9.!")
  (scan-tokens "$#^&!")
  (scan-tokens "var lol_ = \"lol?\" \nclass\n"))
