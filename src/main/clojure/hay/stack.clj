(ns hay.stack)

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

(defprotocol ^:private Item
  (^:private emit      [this])
  (^:private signature [this]))

(extend-protocol Item
  Object
  (emit      [this] #(conj % this))
  (signature [this] [0 1])

  nil
  (emit      [this] #(conj % this))
  (signature [this] [0 1]))
