(ns hay.dev
  (:require
    [clojure.tools.nrepl.server :as repl]
    redl.core
    redl.complete
    ccw.debug.serverrepl))

(reset! redl.core/print-fn prn)

(defn repl-handler
  []
  (repl/default-handler))
