(ns lang.core)

(defn slice
  "Same as clojure `range`, without support for infinite sequences
   or non-integers"
  ([stop] (range 0 stop))
  ([start stop] (range start stop 1))
  ([start stop step] 
   {:pre [(every? int? [start stop step])]}
   {:objtype :objtype/slice :array (range start stop step)}))

(defn ndarray-expr [a f & {:keys [dtype] :or {dtype :dtype/object}}] 
  {:objtype :objtype/array-expr :array a :expr f :dtype dtype})
