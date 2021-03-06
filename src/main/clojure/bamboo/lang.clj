(ns bamboo.lang)

(defn slice [& {:keys [start end]}] 
  {:objtype :objtype/slice :start start :end end})

(defn array-expr [a f & {:keys [dtype] :or {dtype :dtype/object}}] 
  {:objtype :objtype/array-expr :array a :expr f :dtype dtype})

