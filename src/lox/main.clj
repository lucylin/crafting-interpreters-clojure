(ns lox.main
  (:require [lox.scanner :as scanner]))

(defn run
  "Run the given code string"
  [code]
  (scanner/scan-tokens code))

(defn- run-file
  "Load the file at path and run it"
  [path]
  (run (slurp path)))

(defn- run-prompt
  "Read input from the cmdline until there is none"
  []
  (loop []
    (print "> ")
    (flush)
    (let [line (read-line)]
      (when (some? line)
        (run line)
        (recur)))))

(defn -main
  "Load & run the given file, or run interactively"
  [& args]
  (let [nargs (count args)]
    (cond
      (> nargs 1) (do
                    (println "usage: lox [script]")
                    (System/exit 64))
      (= nargs 1) (run-file (first args))
      :else (run-prompt))))
