(ns hay.stack)

(def world
  (atom
    {::namespaces {}
     ::namespace  :haystack.core}))

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
