(ns hay.stack
  (:refer-clojure
    :exclude [read-string eval resolve])
  (:import
    java.util.regex.Pattern
    clojure.lang.AFunction
    clojure.lang.APersistentMap
    clojure.lang.APersistentSet
    clojure.lang.APersistentVector
    clojure.lang.PersistentList
    clojure.lang.Symbol)
  (:require
    [hay.grammar :as g]
    [clojure.java.io :as io]
    [instaparse.core :as insta]
    [net.cgrand.megaatom :as mega]))

(alias 'clj 'clojure.core)

(defn ^:private lookup
  [sym env]
  (let [runtime     (::runtime env)
        nspace      (namespace sym)
        word        (name sym)
        aliases     (get-in runtime [:namespaces (:namespace env) :aliases])
        lookup-path #(vector :namespaces % :words word)
        candidates  (list
                      nspace
                      (when-not nspace (:namespace env))
                      (get aliases nspace))
        resolved    (first
                      (for [candidate candidates
                            :when candidate]
                        (when-let [resolved (get-in runtime
                                                    (lookup-path candidate))]
                          resolved)))]
    (if resolved
      resolved
      (throw (ex-info "Unknown word" {:unkown-word sym})))))

(defn ^:private signature>args
  [sig]
  (cond
    (vector? sig)  (into [] (take-while #(not= % '--) sig))
    (= sig :stack) '[stack]
    (= sig :env)   '[{:keys [stack] :as env}]))

(defn ^:private compile-signature
  [sig]
  (let [to-pop (count (signature>args sig))]
    [to-pop (when-not (= (peek sig) '???) (- (count sig) to-pop 1))]))

(defn ^:private pop-n
  [stack n]
  [(subvec stack (- (count stack) n)) (subvec stack 0 (- (count stack) n))])

(def world
  (atom {::runtime {:namespaces {}}}))

(def runtime
  (mega/subatom world [::runtime]))

(defrecord QuotedSymbol [sym])

(defmethod print-method QuotedSymbol
  [s w]
  (.write w "'")
  (print-method (:sym s) w))

(defrecord QualifiedKeyword [kw])

(defmethod print-method QualifiedKeyword
  [s w]
  (doto w
    (.write "::")
    (.write (name (:kw s)))))

(defrecord Block [words])

(defmethod print-method Block
  [b w]
  (let [n (count (:words b))]
    (doto w
      (.write "'[")
      (.write "Hay block (")
      (.write (cond
                (zero? n) "zero words"
                (= n 1)   "one word"
                :else     (format "%d words" n)))
      (.write ")]"))))

(def ^:private parse (insta/parser g/grammar :start :exprs))

(defn read-string
  [reader]
  (insta/transform
    {:nil      (constantly nil)
     :boolean  #(= % "true")
     :string   #(-> (apply str %&)
                  (.replaceAll "\\\\n" "\n")
                  (.replaceAll "\\\\r" "\t")
                  (.replaceAll "\\\\r" "\t")
                  (.replaceAll "\\\\\"" "\"")
                  (.replaceAll "\\\\\\\\" "\\\\"))
     :regex    #(Pattern/compile %)
     :symbol   (comp symbol str)
     :qsymbol  (fn [nspace sym] (symbol (name nspace) (name sym)))
     :quoted   ->QuotedSymbol
     :keyword  (comp keyword str)
     :qkeyword (fn [nspace & kw]
                 (->QualifiedKeyword
                   (if (symbol? nspace)
                     (keyword (name nspace) (apply str kw))
                     (keyword (apply str nspace kw)))))
     :number   (fn ([number] number) ([sign number] (sign number)))
     :integer  #(Long/parseLong (apply str %&))
     :float    #(Double/parseDouble (apply str %&))
     :rational /
     :sign     {"+" + "-" -}
     :vector   vector
     :list     list
     :set      #(set %&)
     :map      hash-map
     :block    #(->Block %&)}
    (parse reader)))

(defn eval
  [env word]
  (word env))

(defprotocol Word
  (-emit [this env]))

(defprotocol Collection
  (resolve [this env]))

(defn emit
  [word env]
  (let [word (-emit word env)]
    (if (= (::signature (meta word)) :env)
      word
      (recur word env))))

(defn emit-value
  [v]
  ^{::signature :env}
  #(update-in % [:stack] conj v))

(defn emit-fn
  [this]
  (let [sig (-> this meta ::signature)]
    (cond
      (vector? sig)
      (let [[to-pop to-push] (compile-signature sig)]
        ^{::signature :stack}
        (fn [stack]
          (let [[args stack] (pop-n stack to-pop)
                result       (apply this args)]
            (cond
              (nil? to-push)  result
              (zero? to-push) stack
              (= to-push 1)   (conj stack result)
              :else           (into stack result)))))

      (= sig :stack)
      ^{::signature :env}
      #(update-in % [:stack] this)

      (= sig :env)   this
      :else          (throw (ex-info "Function has no valid signature"
                                     {:invalid-signature sig})))))

(defn emit-collection
  [this env]
  (emit-value (resolve this env)))

(extend-protocol Word
  Object
  (-emit [this _env] (emit-value this))

  nil
  (-emit [this _env] (emit-value this))

  Symbol
  (-emit [this _env]
    ^{::signature :env}
    #(eval % (lookup this %)))

  AFunction
  (-emit [this _env] (emit-fn this))

  APersistentMap
  (-emit [this env] (emit-collection this env))

  APersistentSet
  (-emit [this env] (emit-collection this env))

  APersistentVector
  (-emit [this env] (emit-collection this env))

  PersistentList
  (-emit [this env] (emit-collection this env))

  Block
  (-emit [{words :words} env]
    (let [words (map #(emit % env) words)]
      (emit-value ^{::signature :env} #(reduce eval % words))))

  QuotedSymbol
  (-emit [{sym :sym} _env]
    ^{::signature :env}
    #(update-in % [:stack] conj (lookup sym %)))

  QualifiedKeyword
  (-emit [this env] (emit-value (resolve this env))))

(defn ^:private resolve-in
  [env]
  #(resolve % env))

(extend-protocol Collection
  APersistentMap
  (resolve [this env]
    (reduce-kv (fn [this k v]
                 (assoc this (resolve k env) (resolve v env)))
               {} this))

  APersistentSet
  (resolve [this env] (into #{} (map (resolve-in env) this)))

  APersistentVector
  (resolve [this env] (mapv (resolve-in env) this))

  PersistentList
  (resolve [this env] (apply list (map (resolve-in env) this)))

  QuotedSymbol
  (resolve [{sym :sym} env] (lookup sym env))

  QualifiedKeyword
  (resolve [{kw :kw} env]
    (keyword (or (namespace kw) (:namespace env)) (name kw)))

  Block
  (resolve [this env] (emit this env))

  Object
  (resolve [this _env] this)

  nil
  (resolve [this _env] this))

(defn repl
  []
  (loop [stack  []
         nspace "haystack.core"]
    (print (str nspace "=> "))
    (flush)
    (let [words (read-string (read-line))
          env   @world
          prep  (assoc env :stack stack :namespace nspace)
          {:keys [stack namespace] :as new-env}
          (reduce (fn [e w] (eval e (emit w e))) prep words)]
      (reset! world (dissoc new-env :stack :namespace))
      (when-not (= (peek stack) :haystack/exit)
        (println "---Stack---")
        (doseq [v stack] (prn v))
        (recur stack namespace)))))

(defn word-fn
  [sig f]
  (vary-meta f assoc ::signature sig))

(defmacro word
  [sig & body]
  `(word-fn '~sig (fn ~(signature>args sig) ~@body)))

(def empty-namespace
  {:words   {}
   :aliases {nil "haystack.core"}})

(defn create-namespace
  [env nspace]
  (update-in env [::runtime :namespaces (name nspace)]
             (fnil identity empty-namespace)))

(defmacro with-hay-ns
  [nspace & body]
  `(swap! world (fn [env#]
                  (let [nspace# (name ~nspace)]
                    (-> env#
                      (create-namespace nspace#)
                      (assoc :namespace nspace#)
                      ~@body
                      (dissoc :namespace))))))

(defn defhay
  [env w word]
  (assoc-in env [::runtime :namespaces (:namespace env) :words (name w)]
            (emit word env)))

(defmacro defhayfn
  [env w sig & body]
  `(defhay ~env ~w (vary-meta (fn ~(signature>args sig) ~@body)
                              assoc ::signature '~sig)))

(defn ^:private coll
  [ctor stack]
  (let [n (peek stack)
        [args stack] (pop-n (pop stack) n)]
    (conj stack (ctor args))))

(with-hay-ns :haystack.core
  (defhayfn :.
    :env
    (let [[[w block] stack] (pop-n stack 2)]
      (-> env
        (assoc :stack stack)
        (assoc-in [::runtime :namespaces (:namespace env) :words (name w)]
                  block))))

  (defhayfn :in-ns
    :env
    (let [nspace (name (peek stack))]
      (-> env
        (create-namespace nspace)
        (assoc :namespace nspace)
        (assoc :stack (pop stack)))))

  (defhayfn :apply
    :env
    (let [[[f] stack] (pop-n stack 1)]
      (-> env
        (assoc :stack stack)
        (eval f))))

  (defhayfn :if
    :env
    (let [[[test-word then-word else-word] stack] (pop-n stack 3)
          {:keys [stack] :as env} (eval (assoc env :stack stack) test-word)]
      (if (peek stack)
        (eval (update-in env [:stack] pop) then-word)
        (eval (update-in env [:stack] pop) else-word))))

  (defhay :identity (word-fn '[a -- a] identity))

  (defhay :even? (word-fn '[n -- bool] even?))
  (defhay :odd?  (word-fn '[n -- bool] odd?))
  (defhay :+     (word-fn '[n m -- num] +))
  (defhay :add   (word-fn '[n m -- num] +))
  (defhay :-     (word-fn '[n m -- num] -))
  (defhay :sub   (word-fn '[n m -- num] -))
  (defhay :*     (word-fn '[n m -- num] *))
  (defhay :mult  (word-fn '[n m -- num] *))
  (defhay :div   (word-fn '[n m -- num] /))

  (defhayfn :vector     :stack (coll vec stack))
  (defhayfn :list       :stack (coll #(apply list %) stack))
  (defhayfn :hash-map   :stack (coll #(apply hash-map %) stack))
  (defhayfn :sorted-map :stack (coll #(apply sorted-map %) stack))
  (defhayfn :hash-set   :stack (coll set stack))
  (defhayfn :sorted-set :stack (coll #(apply sorted-set %) stack))

  (defhay :conj   (word-fn '[coll x -- coll]   conj))
  (defhay :disj   (word-fn '[coll x -- coll]   disj))
  (defhay :assoc  (word-fn '[coll k v -- coll] assoc))
  (defhay :dissoc (word-fn '[coll k -- coll]   dissoc))
  (defhay :count  (word-fn '[coll -- n]        count))

  (defhayfn :dup    :stack (conj stack (peek stack)))
  (defhayfn :forget :stack (pop stack))
  (defhayfn :swap   :stack (let [[[x y] stack] (pop-n stack 2)]
                             (-> stack (conj y) (conj x)))))

