(ns hay.stack
  (:refer-clojure
    :exclude [read-string eval resolve]
    :as clj)
  (:import
    java.util.regex.Pattern
    clojure.lang.AFunction
    clojure.lang.APersistentMap
    clojure.lang.APersistentSet
    clojure.lang.APersistentVector
    clojure.lang.PersistentList
    clojure.lang.Symbol
    clojure.lang.Var)
  (:require
    [clojure.java.io :as io]
    [instaparse.core :as insta]
    [net.cgrand.megaatom :as mega]))

(def ^:dynamic *namespace* "haystack.core")

(def world
  (atom {::runtime {:namespaces {}}}))

(def runtime
  (mega/subatom world [::runtime]))

(defn resolve-sym
  [sym]
  (let [runtime     @runtime
        nspace      (namespace sym)
        word        (name sym)
        aliases     (get-in runtime [:namespaces *namespace* :aliases])
        lookup-path #(vector :namespaces % :words word)
        candidates  (list
                      nspace
                      (when-not nspace *namespace*)
                      (get aliases nspace))
        nspace      (first
                      (for [candidate candidates
                            :when candidate
                            :when (get-in runtime (lookup-path candidate))]
                        candidate))]
    (if nspace
      (symbol nspace word)
      (throw (ex-info "Unknown word" {:unkown-word sym})))))

(defrecord QuotedSymbol [sym])

(defmethod print-method QuotedSymbol
  [s w]
  (doto w
    (.write "'")
    (.write (name (:sym s)))))

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

(def ^:private parse
  (insta/parser (io/resource "hay/grammar.ebnf")))

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
                 (let [[nspace kw] (if (symbol? nspace)
                                     [(name nspace) (apply str kw)]
                                     [(name *namespace*)
                                      (apply str nspace kw)])]
                   (keyword nspace kw)))
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
  [stack word]
  (word stack))

(defprotocol Word
  (emit [this]))

(defprotocol Collection
  (resolve [this]))

(defn emit-value
  [v]
  ^{::signature :stack}
  #(conj % v))

(declare pop-n compile-signature lookup)

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

      (= sig :stack) this
      :else          (throw (ex-info "Function has no valid signature"
                                     {:invalid-signature sig})))))

(defn emit-collection
  [this]
  (emit-value (resolve this)))

(extend-protocol Word
  Object
  (emit [this] (emit-value this))

  nil
  (emit [this] (emit-value this))

  Symbol
  (emit [this]
    ^{::signature :stack}
    #(eval % (lookup this)))

  AFunction
  (emit [this] (emit-fn this))

  APersistentMap
  (emit [this] (emit-collection this))

  APersistentSet
  (emit [this] (emit-collection this))

  APersistentVector
  (emit [this] (emit-collection this))

  PersistentList
  (emit [this] (emit-collection this))

  Var
  (emit [this] (emit-fn this))

  Block
  (emit [{words :words}]
    (let [words (map emit words)]
      (emit-value ^{::signature :stack} #(reduce eval % words))))

  QuotedSymbol
  (emit [{sym :sym}]
    ^{::signature :stack}
    #(conj % (lookup sym))))

(extend-protocol Collection
  APersistentMap
  (resolve [this]
    (reduce-kv (fn [this k v] (assoc this (resolve k) (resolve v))) {} this))

  APersistentSet
  (resolve [this] (into #{} (map resolve this)))

  APersistentVector
  (resolve [this] (mapv resolve this))

  PersistentList
  (resolve [this] (apply list (map resolve this)))

  QuotedSymbol
  (resolve [{sym :sym}] (lookup sym))

  Block
  (resolve [this] (emit this))

  Object
  (resolve [this] this)

  nil
  (resolve [this] this))

(defn repl
  []
  (binding [*namespace* *namespace*]
    (loop [stack []]
      (print (str *namespace* "=> "))
      (flush)
      (let [text  (read-line)
            stack (->> (read-string text)
                    (map emit)
                    (reduce eval stack))]
        (when-not (= (peek stack) :haystack/exit)
          (println "---Stack---")
          (doseq [v stack] (prn v))
          (recur stack))))))

(declare signature>args)

(defn word-fn
  [sig f]
  (vary-meta f assoc ::signature sig))

(defmacro word
  [sig & body]
  `(word-fn '~sig (fn ~(signature>args sig) ~@body)))

(def empty-namespace
  {:words   {}
   :aliases {nil "haystack.core"}})

(defn create-namespace!
  [nspace]
  (mega/swap-in! runtime [:namespaces nspace]
                 (fnil identity empty-namespace)))

(defmacro with-hay-ns
  [nspace & body]
  `(binding [*namespace* ~(name nspace)]
     (create-namespace! *namespace*)
     ~@body))

(defn defhay
  [w word]
  (mega/swap-in! runtime [:namespaces *namespace* :words]
                 assoc (name w) (emit word)))

(defmacro defhayfn
  [w sig & body]
  `(defhay ~w (vary-meta (fn ~(signature>args sig) ~@body)
                         assoc ::signature '~sig)))

(with-hay-ns :haystack.core
  (defhayfn :.
    [w block --]
    (mega/swap-in! runtime [:namespaces *namespace* :words]
                   assoc (name w) block))

   (defhayfn :in-ns
     [nspace --]
     (let [nspace (name nspace)]
       (create-namespace! nspace)
       (set! *namespace* nspace)))

  (defhayfn :apply
    :stack
    (let [[[f] stack] (pop-n stack 1)]
      (eval stack f)))

  (defhayfn :if
    :stack
    (let [[[test-word then-word else-word] stack] (pop-n stack 3)
          stack (eval stack test-word)]
      (if (peek stack)
        (eval (pop stack) then-word)
        (eval (pop stack) else-word))))

  (defhay :identity (word-fn '[a -- a] identity))

  (defhay :even? (word-fn '[n -- bool] even?))
  (defhay :odd?  (word-fn '[n -- bool] odd?))
  (defhay :+     (word-fn '[n m -- num] +))
  (defhay :add   (word-fn '[n m -- num] +))
  (defhay :-     (word-fn '[n m -- num] -))
  (defhay :sub   (word-fn '[n m -- num] -))
  (defhay :*     (word-fn '[n m -- num] *))
  (defhay :mult  (word-fn '[n m -- num] *))
  (defhay :div   (word-fn '[n m -- num] /)))

(defn ^:private lookup
  [word]
  (let [word (resolve-sym word)]
    (get-in @runtime [:namespaces (namespace word) :words (name word)])))

(defn ^:private signature>args
  [sig]
  (if (vector? sig)
    (into [] (take-while #(not= % '--) sig))
    '[stack]))

(defn ^:private compile-signature
  [sig]
  (let [to-pop (count (signature>args sig))]
    [to-pop (when-not (= (peek sig) '???) (- (count sig) to-pop 1))]))

(defn ^:private pop-n
  [stack n]
  [(subvec stack (- (count stack) n)) (subvec stack 0 (- (count stack) n))])
