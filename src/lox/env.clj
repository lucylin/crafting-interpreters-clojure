(ns lox.env
  (:require [lox.error :as error]))

(defn ->lox-env [] {:parent nil :vars {}})

(defn assign-var
  [env var-name var-value]
  (cond
    (nil? env) (error/runtime-ex var-name "Undefined variable")

    (contains? (:vars env) (:lexeme var-name))
    (assoc-in env [:vars (:lexeme var-name)] var-value)

    :else (assoc env :parent (assign-var (:parent env) var-name var-value))))

(defn declare-var
  ([env var-name] (declare-var env var-name nil))

  ([env var-name var-value]
   (assoc-in env [:vars (:lexeme var-name)] var-value)))

(defn get-var
  [env var-name]
  (cond
    (nil? env) (error/runtime-ex var-name "Undefined variable")

    (contains? (:vars env) (:lexeme var-name))
    (get-in env [:vars (:lexeme var-name)])

    :else (get-var (:parent env) var-name)))

(comment
  (get-var (declare-var (->lox-env) {:lexeme "foo"} "bar") {:lexeme "foo"}))
