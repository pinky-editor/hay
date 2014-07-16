(ns hay.stack
  (:refer-clojure
    :exclude [read-string]
    :as clj)
  (:import
    java.util.regex.Pattern
    clojure.lang.Symbol
    clojure.lang.Var)
  (:require
    [clojure.java.io :as io]
    [instaparse.core :as insta]))

(def world
  (atom
    {::namespaces {}
     ::namespace  :haystack.core}))

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

(defn ^:private lookup
  [word]
  (let [world @world]
    (if-let [w (get-in world [:namespaces (::namespace world) word])]
      w
      (throw (ex-info "Unknown word" {:unkown-word word})))))

(defn ^:private compile-signature
  [sig]
  (reduce (fn [[to-push _] x]
            (if (= x '--)
              (reduced [to-push (- (count sig) to-push 1)])
              [(inc to-push) _]))
          [0 0] sig))

(defn ^:private pop-n
  [stack n]
  (nth (iterate (fn [[popped stack]]
                     [(conj popped (peek stack)) (pop stack)])
                [[] stack])
       n))

(defprotocol ^:private Item
  (^:private emit [this]))

(extend-protocol Item
  Object
  (emit [this] #(conj % this))

  nil
  (emit [this] #(conj % this))

  Symbol
  (emit [this] (emit (lookup this)))

  Var
  (emit [this]
    (let [[to-pop to-push] (-> this meta ::signature compile-signature)]
      (fn [stack]
        (let [[args stack] (pop-n stack to-pop)
              result       (apply @this args)]
          (cond
            (zero? to-push) stack
            (= to-push 1)   (conj stack result)
            :else           (into stack result))))))

  Block
  (emit [{words :words}]
    (let [words (map emit words)]
      (fn [stack]
        (reduce #(%2 %1) stack words))))

  QuotedSymbol
  (emit [{sym :sym}]
    (let [v (lookup sym)]
      #(conj % v))))
