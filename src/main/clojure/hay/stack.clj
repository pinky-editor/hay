;-
; Copyright 2014-2015 © Meikel Brandmeyer.
; All rights reserved.
;
; Licensed under the EUPL V.1.1 (cf. file EUPL-1.1 distributed with the
; source code.) Translations in other european languages available at
; https://joinup.ec.europa.eu/software/page/eupl.
;
; Alternatively, you may choose to use the software under the MIT license
; (cf. file MIT distributed with the source code).

(ns hay.stack
  (:require
    [hay.compiler :as compiler]
    [hay.engine   :as engine]
    [hay.grammar  :as grammar]))

(def core-signatures
  {#'+     '[x y -- z]
   #'conj  '[coll x -- coll]
   #'count '[coll -- n]})

(doseq [[v sig] core-signatures]
  (alter-meta! v assoc :hay/signature sig)
  (compiler/expose-var v))

(defn ^:private sig>args
  [sig]
  (if (= sig :thread)
    '[thread]
    (vec (take-while #(not= % '--) sig))))

(defn setword!
  [nspace wname value]
  (swap! engine/runtime assoc-in [:namespaces nspace :words wname] value))

(defmacro defword
  [fname & {:keys [namespace signature body] :or {namespace (ns-name *ns*)}}]
  `(setword! ~(name namespace) ~(name fname)
             (-> (fn ~(sig>args signature) ~body)
               (vary-meta assoc :hay/signature '~signature)
               compiler/compile)))

(defword :map
  :signature [f s -- s]
  :body
  (map (engine/hay-call f) s))

(defword :.
  :signature :thread
  :body
  (let [[stack [block word]] (engine/pop-n (:stack thread) 2)]
    (setword! (get-in thread [:locals "*ns*"]) (name word) block)
    (assoc thread :stack stack)))

(defn repl
  []
  (loop [t (assoc-in (engine/>hay-thread nil)
                     [:locals "*ns*"] "user")]
    (print (str (get-in t [:locals "*ns*"]) "⇒ "))
    (flush)
    (let [words (map compiler/compile (grammar/read-string (read-line)))
          ins   (mapcat :instructions words)
          t     (engine/run (assoc t :instructions ins :state :running))
          stack (:stack t)]
      (when-not (= (peek stack) :hay/exit)
        (doseq [v stack] (print " → ") (prn v))
        (recur t)))))
