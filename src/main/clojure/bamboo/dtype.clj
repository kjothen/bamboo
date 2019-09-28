(ns bamboo.dtype)

(def bamboo-hierarchy (-> (make-hierarchy)
                          (derive :dtype/index :dtype/indices)
                          (derive :dtype/rangeindex :dtype/indices)
                          (derive :dtype/datetimeindex :dtype/indices)))
