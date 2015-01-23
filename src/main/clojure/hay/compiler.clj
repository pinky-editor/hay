;-
; Copyright 2015 © Meikel Brandmeyer.
; All rights reserved.
;
; Licensed under the EUPL V.1.1 (cf. file EUPL-1.1 distributed with the
; source code.) Translations in other european languages available at
; https://joinup.ec.europa.eu/software/page/eupl.
;
; Alternatively, you may choose to use the software under the MIT license
; (cf. file MIT distributed with the source code).

(ns hay.compiler
  (:refer-clojure :exclude [compile])
  (:require
    hay.grammar)
  (:import
    clojure.lang.AFn
    clojure.lang.Symbol
    clojure.lang.Var
    hay.grammar.Block))

(def CALL     [:CALL])
(def NO-OP    [:NO-OP])
(def PUSH     [:PUSH])
(def PUSH-ALL [:PUSH-ALL])
(def POP      [:POP])

(defprotocol Compileable
  (-compile [this]))

(defrecord Compilate [instructions])

(defn ^:private compile-signature
  [sig]
  (let [n       (count sig)
        to-pop  (or (some (fn [[idx sym]] (when (= sym '--) idx))
                          (map-indexed vector sig))
                    n)
        to-push (max (- n to-pop 1) 0)]
    [to-pop to-push]))

(defn ^:private compile-fn
  [f]
  (let [signature (:hay/signature (meta f))]
    (cond
      (vector? signature)
      (let [[to-pop to-push] (compile-signature signature)]
        [[:POP to-pop]
         [:WORD f]
         CALL
         (case to-push
           0 NO-OP
           1 PUSH
           PUSH-ALL)])

      (nil? signature)
      (throw
        (ex-info "cannot compile clojure function without signature" {:fn f}))

      :else
      (throw
        (ex-info "cannot compile clojure function with invalid signature"
                 {:fn f :signature signature})))))

(extend-protocol Compileable
  Compilate
  (-compile [this] (:instructions this))

  Object
  (-compile [this] [[:VALUE this] PUSH])

  nil
  (-compile [this] [[:VALUE nil] PUSH])

  AFn
  (-compile [this] (compile-fn this))

  Var
  (-compile [this]
    (if (ifn? @this)
      (compile-fn this)
      [[:VALUE this] [:WORD #'clojure.core/deref] [:CALL] [:PUSH]]))

  Block
  (-compile [this] (reduce into [] (map -compile (:words this))))

  Symbol
  (-compile [this] [[:LOOKUP this]]))

(defn compile
  [word]
  (->Compilate (-compile word)))