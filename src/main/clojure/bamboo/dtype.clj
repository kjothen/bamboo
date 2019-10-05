(ns bamboo.dtype)

(def bamboo-hierarchy (-> (make-hierarchy)
                          (derive :dtype/rangeindex :dtype/index)
                          (derive :dtype/datetimeindex :dtype/index)))
