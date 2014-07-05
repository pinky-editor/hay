(ns hay.stack
  (:import
    clojure.lang.Symbol
    clojure.lang.Var))

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

(defn ^:private pop-n
  [stack n]
  (nth (iterate (fn [[popped stack]]
                     [(conj popped (peek stack)) (pop stack)])
                [[] stack])
       n))

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
  (signature [this] (signature (lookup this)))

  Var
  (emit [this]
    (let [[to-pop to-push] (signature this)]
      (fn [stack]
        (let [[args stack] (pop-n stack to-pop)
              result       (apply @this args)]
          (cond
            (zero? to-push) stack
            (= to-push 1)   (conj stack result)
            :else           (into stack result))))))
  (signature [this] (-> this meta ::signature compile-signature)))
