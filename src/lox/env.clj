(ns lox.env)

(defn ->lox-env [] {:parent nil :vars {}})

(defn assign-var
  [env var-name var-value]
  ; TODO: figure out error handling if var does not exist
  (assoc-in env [:vars (:lexeme var-name)] var-value))

(defn declare-var
  ([env var-name] (declare-var env var-name nil))

  ([env var-name var-value]
   (assoc-in env [:vars (:lexeme var-name)] var-value)))

(defn get-var
  [env var-name]
  (cond
    (nil? env) nil
    (contains? (:vars env) (:lexeme var-name)) (get-in env [:vars (:lexeme var-name)])
    :else (get-var (:parent env) var-name)))

(comment
  (get-var (declare-var (->lox-env) {:lexeme "foo"} "bar") {:lexeme "foo"}))
