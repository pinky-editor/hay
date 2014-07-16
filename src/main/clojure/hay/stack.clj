(ns hay.stack
  (:refer-clojure
    :exclude [read-string eval]
    :as clj)
  (:import
    java.util.regex.Pattern
    clojure.lang.AFunction
    clojure.lang.Symbol
    clojure.lang.Var)
  (:require
    [clojure.java.io :as io]
    [instaparse.core :as insta]))

(def ^:dynamic *namespace* :haystack.core)

(def world
  (atom
    {::namespaces {}}))

(defrecord QuotedSymbol [sym])
(defrecord Block [words])

(def ^:private parse
  (insta/parser (io/resource "hay/grammar.ebnf")))

(defn read-string
  [reader]
  (insta/transform
    {:unit     list
     :nil      (constantly nil)
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
                                     [(name (get @world ::namespace))
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

(defprotocol ^:private Word
  (^:private emit [this]))

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

(extend-protocol Word
  Object
  (emit [this] (emit-value this))

  nil
  (emit [this] (emit-value this))

  Symbol
  (emit [this] (lookup this))

  AFunction
  (emit [this] (emit-fn this))

  Var
  (emit [this] (emit-fn this))

  Block
  (emit [{words :words}]
    (let [words (map emit words)]
      (emit-value ^{::signature :stack} #(reduce eval % words))))

  QuotedSymbol
  (emit [{sym :sym}] (emit-value (lookup sym))))

(declare signature>args)

(defmacro word
  [sig & body]
  `(with-meta (fn ~(signature>args sig) ~@body) {::signature '~sig}))

(defmacro defword
  [name sig & body]
  `(do
     (def ~name (word ~sig ~@body))
     (alter-meta! (var ~name) assoc ::signature '~sig)
     (var ~name)))

(def empty-namespace {:words {}})

(defn create-namespace
  [nspace]
  (swap! world update-in [::namespaces nspace]
         (fnil identity empty-namespace)))

(defn map-words
  [hay-nspace mappings]
  (create-namespace hay-nspace)
  (swap! world update-in [::namespaces hay-nspace]
         (fn [nspace]
           (reduce (fn [nspace [n v]]
                     (assoc-in nspace [:words n] (emit v)))
                   nspace
                   mappings))))

(defn map-namespace
  ([clj-nspace] (map-namespace clj-nspace (keyword (name clj-nspace))))
  ([clj-nspace hay-nspace]
   (map-words hay-nspace (keep (fn [[n v]]
                                 (when (contains? (meta v) ::signature)
                                   [(keyword (name n)) v]))
                               (ns-publics (the-ns clj-nspace))))))

(defn ^:private lookup
  [word]
  (if-let [w (get-in @world [::namespaces *namespace*
                             :words (keyword (name word))])]
    w
    (throw (ex-info "Unknown word" {:unkown-word word}))))

(defn ^:private signature>args
  [sig]
  (into [] (take-while #(not= % '--) sig)))

(defn ^:private compile-signature
  [sig]
  (let [to-pop (count (signature>args sig))]
    [to-pop (when-not (= (peek sig) '???) (- (count sig) to-pop 1))]))

(defn ^:private pop-n
  [stack n]
  [(subvec stack (- (count stack) n)) (subvec stack 0 (- (count stack) n))])
