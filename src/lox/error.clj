(ns lox.error)

(defn runtime-ex
  [token message]
  (throw
   (ex-info message
            {:type :runtime-error :line (:line token) :token (:type token)})))
