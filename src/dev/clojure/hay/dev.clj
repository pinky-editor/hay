(ns hay.dev
  (:require
    [clojure.tools.nrepl.server :as repl]
    redl.core
    redl.complete))

(reset! redl.core/pretty-print false)

(defn repl-handler
  []
  (repl/default-handler))
