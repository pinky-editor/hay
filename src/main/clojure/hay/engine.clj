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

(ns hay.engine
  (:require
    hay.compiler
    hay.grammar)
  (:import
    clojure.lang.AFunction
    hay.compiler.Compilate
    hay.grammar.Block))

(def runtime (atom {:namespaces {}}))

(defrecord HayThread
  [stack
   value
   word
   state
   instructions
   locals])

(defn >hay-thread
  [compilate]
  (->HayThread [] nil nil :running
               (:instructions compilate)
               {"*ns*" "hay.stack"}))

(defmulti evaluate
  (fn [_thread instruction] (nth instruction 0))
  :default :NO-OP)

(defn step
  [thread]
  (if-let [[op & more-instructions] (seq (:instructions thread))]
    (-> thread
      (assoc :instructions more-instructions)
      (evaluate op))
    (assoc thread :state :done)))

(defn run
  [thread]
  (if (= (:state thread) :running)
    (recur (step thread))
    thread))

(defn freeze
  [thread]
  (assoc thread :state :frozen))

(defn thaw
  [thread]
  (assoc thread :state :running))

(defmethod evaluate
  :NO-OP
  [thread _]
  thread)

(defmethod evaluate
  :FREEZE
  [thread _]
  (freeze thread))

(defn pop-n
  [stack n]
  (case n
    0 [stack []]
    1 [(pop stack) [(peek stack)]]
    (reduce (fn [[stack values] _] [(pop stack) (conj values (peek stack))])
            [stack []] (range n))))

(defmethod evaluate
  :POP
  [thread [_ n]]
  (let [n               (or n 1)
        [nstack values] (pop-n (:stack thread) n)]
    (-> thread
      (assoc :stack nstack)
      (assoc :value values))))

(defmethod evaluate
  :PUSH
  [thread _]
  (-> thread
    (update-in [:stack] conj (:value thread))
    (assoc :value nil)))

(defmethod evaluate
  :PUSH-ALL
  [thread _]
  (-> thread
    (update-in [:stack] into (:value thread))
    (assoc :value nil)))

(defmethod evaluate
  :VALUE
  [thread [_ v]]
  (assoc thread :value v))

(defmethod evaluate
  :WORD
  [thread [_ f]]
  (assoc thread :word f))

(defprotocol ICall
  (call [this thread]))

(extend-protocol ICall
  AFunction
  (call [this thread]
    (letfn [(do-call [v]
              (case (count v)
                0 (this)
                1 (this (nth v 0))
                2 (this (nth v 0) (nth v 1))
                3 (this (nth v 0) (nth v 1) (nth v 2))
                4 (this (nth v 0) (nth v 1) (nth v 2) (nth v 3))
                5 (this (nth v 0) (nth v 1) (nth v 2) (nth v 3) (nth v 4))
                (apply this v)))]
      (update-in thread [:value] do-call)))

  Compilate
  (call [this thread]
    (-> this
      :instructions
      (concat (:instructions thread))
      (->> (assoc thread :instructions)))))

(defmethod evaluate
  :CALL
  [thread _]
  (call (:word thread) thread))

(defmethod evaluate
  :THREAD-CALL
  [thread [_ f]]
  (f thread))

(defn ^:private >word
  [x]
  {:post [(instance? Compilate %)]}
  (cond
    (vector? x) (nth x 1)
    (instance? Compilate x) x
    :else (or (:hay/compilate (meta x)) (deref x))))

(defmethod evaluate
  :LOOKUP
  [thread [_ sym]]
  (let [nspace (namespace sym)
        sspace (or nspace (get (:locals thread) "*ns*"))
        name   (name sym)]
    (if-let [w (or (when-not nspace (find (:locals thread) name))
                   (get-in @runtime [:namespaces sspace :words name])
                   (get (ns-publics (symbol sspace)) (symbol name))
                   (get-in @runtime [:namespaces "hay.stack" :words name])
                   (get (ns-publics 'clojure.core) (symbol name)))]
      (assoc thread :instructions (concat [[:WORD (>word w)] hay.compiler/CALL]
                                          (:instructions thread)))
      (throw
        (ex-info (str "unknown word: " name) {:sym sym})))))
