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

(ns hay.grammar
  (:refer-clojure
    :exclude [+ - * read-string])
  (:import
    java.util.regex.Pattern
    clojure.lang.APersistentSet
    clojure.lang.APersistentVector
    clojure.lang.PersistentList
    clojure.lang.Keyword)
  (:require
    [instaparse.combinators :as insta]
    [instaparse.core :as insta-c]))

(alias 'clj 'clojure.core)

(defprotocol GrammarItem
  (translate [this]))

(extend-protocol GrammarItem
  Object
  (translate [this] this)

  nil
  (translate [this] insta/Epsilon)

  String
  (translate [this] (insta/string this))

  Pattern
  (translate [this] (insta/regexp this))

  APersistentSet
  (translate [this] (apply insta/alt (map translate this)))

  APersistentVector
  (translate [this] (apply insta/cat (map translate this)))

  Keyword
  (translate [this]
    (let [this         ^String (name this)
          [start hide] (if (.startsWith this "-")
                         [1 insta/hide]
                         [0 identity])
          [end modify] (cond
                         (.endsWith this "+") [1 insta/plus]
                         (.endsWith this "*") [1 insta/star]
                         (.endsWith this "?") [1 insta/opt]
                         :else [0 identity])]
      (-> this
        (subs start (clj/- (count this) end))
        keyword
        insta/nt
        modify
        hide))))

(defn pimp-my-grammar
  [grammar-map]
  (reduce-kv (fn [m k v]
               (let [kn ^String (name k)]
                 (if (.startsWith kn "-")
                   (assoc m (keyword (name (subs kn 1)))
                          (insta/hide-tag (translate v)))
                   (assoc m k (translate v)))))
             {} grammar-map))

(def ?  (comp insta/opt  translate))
(def *  (comp insta/star translate))
(def +  (comp insta/plus translate))
(def -  (comp insta/hide translate))
(def >> (comp insta/look translate))
(def !> (comp insta/neg  translate))

(defn list-of
  [elem-nt]
  (let [list-nt (keyword (str (name elem-nt) "s"))]
    #{nil elem-nt [elem-nt :-ws+ list-nt]}))

(defn coll-of
  [left right elem-nt]
  [(- left) :-ws* elem-nt :-ws* (- right)])

(def grammar
  (pimp-my-grammar
    {:ws        #"[ \t\r\n,]"
     :-exprs    (list-of :expr)
     :-literals (list-of :literal)

     :-expr     #{:symbol :qsymbol :literal}
     :-literal  #{:nil :boolean :number :string :regex
                  :quoted :keyword :qkeyword
                  :vector :list :set :map :block}

     :nil       (- "nil")
     :boolean   #{"true" "false"}
     :string    [(- "\"") #"([^\"\\]|\\.)*" (- "\"")]
     :regex     ["#" :string]

     :symbol    [:symhead :symrest*]
     :qsymbol   [:symbol (- "/") :symbol]
     :quoted    [(- "'") :symbol]
     :keyword   [(- ":") :symrest+]
     :qkeyword  #{[(- "::") :symrest+] [(- ":") :symbol (- "/") :symrest+]}

     :-symhead  #{#"[a-zA-Z._+*!?<=>]" ["-" (!> #"[0-9]")]}
     :-symrest  #"[a-zA-Z0-9._+*!?<=>#'-]"

     :number    [:sign? #{:integer :float :rational}]
     :integer   #{"0" [#"[1-9]" (* #"[0-9]")]}
     :float     [#{"0" [#"[1-9]" (* #"[0-9]")]} "." (+ #"[0-9]")]
     :rational  [:integer (- "/") :integer]
     :sign      [#"[+-]" (>> #"[0-9]")]

     :vector    (coll-of "["  "]" :literals)
     :list      (coll-of "("  ")" :literals)
     :set       (coll-of "#{" "}" :literals)
     :map       (coll-of "{"  "}" :literals)
     :block     (coll-of "'[" "]" :exprs)}))

(defrecord QuotedSymbol [sym])

(defmethod print-method QuotedSymbol
  [s w]
  (.write w "'")
  (print-method (:sym s) w))

(defrecord QualifiedKeyword [kw])

(defrecord Block [words])

(def ^:private parse (insta-c/parser grammar :start :exprs))

(defn read-string
  [reader]
  (insta-c/transform
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
