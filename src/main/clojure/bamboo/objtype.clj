(ns bamboo.objtype)

(def bamboo-hierarchy (-> (make-hierarchy)
                          (derive :objtype/rangeindex :objtype/index)
                          (derive :objtype/datetimeindex :objtype/index)))
