(ns hay.grammar
  (:refer-clojure
    :exclude [+ - *])
  (:import
    java.util.regex.Pattern
    clojure.lang.APersistentSet
    clojure.lang.APersistentVector
    clojure.lang.PersistentList
    clojure.lang.Keyword)
  (:require
    [instaparse.combinators :as insta]))

(alias 'clj 'clojure.core)

(defprotocol GrammarItem
  (translate [this]))

(extend-protocol GrammarItem
  Object
  (translate [this] this)

  nil
  (translate [this] insta/Epsilon)

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
        hide)))

  String
  (translate [this] (insta/string this))

  Pattern
  (translate [this] (insta/regexp this)))

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
 
(defmacro defgrammarfn
  [fn-name args body]
  `(def ~fn-name (fn ~args (translate ~body))))

(defgrammarfn list-of
  [elem-nt]
  (let [list-nt (keyword (str (name elem-nt) "s"))]
    #{nil elem-nt [elem-nt :-ws+ list-nt]}))

(defgrammarfn coll-of
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
