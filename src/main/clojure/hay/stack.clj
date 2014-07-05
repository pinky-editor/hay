(ns hay.stack
  (:import
    clojure.lang.Symbol))

(def world
  (atom
    {::namespaces {}
     ::namespace  :haystack.core}))

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

(defprotocol ^:private Item
  (^:private emit      [this])
  (^:private signature [this]))

(extend-protocol Item
  Object
  (emit      [this] #(conj % this))
  (signature [this] [0 1])

  nil
  (emit      [this] #(conj % this))
  (signature [this] [0 1])

  Symbol
  (emit      [this] (lookup this))
  (signature [this] (signature (lookup this))))
