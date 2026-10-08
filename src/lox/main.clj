(ns lox.main
  (:require [clojure.string :as str]))

(defn- run-file
  "Load the file at path and run it"
  [path]
  (slurp path))  ; TODO: run(contents)

(defn- run-prompt
  "Read input from the cmdline until there is none"
  []
  (loop []
    (print "> ")
    (flush)
    (let [line (read-line)]
      (when (some? line)
        (println line)  ; TODO: run(line)
        (recur)))))

(defn -main
  "Load & run the given file, or run interactively"
  [& args]
  (let [nargs (count args)]
    (cond
      (> nargs 1) (println "usage: lox [script]")
      (= nargs 1) (run-file (first args))
      :else (run-prompt))))
