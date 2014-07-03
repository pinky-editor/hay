(ns hay.dev
  (:require
    [clojure.tools.nrepl.server :as repl]
    redl.core
    redl.complete))

(defn repl-handler
  []
  (repl/default-handler))
