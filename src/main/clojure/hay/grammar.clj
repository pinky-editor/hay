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

(def ?  (comp insta/opt  translate))
(def *  (comp insta/star translate))
(def +  (comp insta/plus translate))
(def -  (comp insta/hide translate))
(def >> (comp insta/look translate))
(def !> (comp insta/neg  translate))

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
