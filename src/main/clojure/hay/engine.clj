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

(ns hay.engine)

(defrecord HayThread
  [stack
   value
   word
   frozen?
   instructions])

(defn >hay-thread
  [instructions]
  (->HayThread [] nil nil false instructions))

(defmulti evaluate
  (fn [_thread instruction] (nth instruction 0))
  :default :NO-OP)

(defn step
  [thread]
  (let [[op & more-instructions] (:instructions thread)]
    (-> thread
      (evaluate op)
      (assoc :instructions more-instructions))))

(defn run
  [thread]
  (cond
    (:frozen? thread)            thread
    (seq (:instructions thread)) (recur (step thread))
    :else
    (let [stack (:stack thread)]
      (case (count stack)
        0 nil
        1 (peek stack)
        stack))))

(defn freeze
  [thread]
  (assoc thread :frozen? true))

(defn thaw
  [thread]
  (assoc thread :frozen? false))

(defmethod evaluate
  :NO-OP
  [thread _]
  thread)

(defmethod evaluate
  :FREEZE
  [thread _]
  (freeze thread))

(defn ^:private pop-n
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

(defmethod evaluate
  :CALL
  [thread _]
  (let [v  (:value thread)
        nv (case (count v)
             0 ((:word thread))
             1 ((:word thread) (nth v 0))
             2 ((:word thread) (nth v 0) (nth v 1))
             3 ((:word thread) (nth v 0) (nth v 1) (nth v 2))
             4 ((:word thread) (nth v 0) (nth v 1) (nth v 2) (nth v 3))
             5 ((:word thread) (nth v 0) (nth v 1) (nth v 2) (nth v 3) (nth v 4))
             (apply (:word thread) v))]
    (assoc thread :value nv)))



