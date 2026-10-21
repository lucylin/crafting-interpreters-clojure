(ns lox.env)

(defn ->lox-env [] {:parent nil :vars {}})

(defn assign-var
  [env var-name var-value]
  (-> env (assoc var-name var-value)))

(defn get-var
  [env var-name]
  (case
   (nil? env) nil
   (contains? var-name (:vars env)) (env var-name)
   :else (get-var (:parent env) var-name)))

