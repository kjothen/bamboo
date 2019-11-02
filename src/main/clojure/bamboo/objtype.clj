(ns bamboo.objtype
  (:require [bamboo.utility :refer [in?]]))

(def bamboo-hierarchy (-> (make-hierarchy)
                          (derive :objtype/rangeindex :objtype/index)
                          (derive :objtype/datetimeindex :objtype/index)))

(defn scalar? [x] ((some-fn int? double? boolean? string?) x))

(defn ndarray? [a] 
  (in? (:dtype a) #{:dtype/bool :dtype/float64 :dtype/int64 :dtype/object}))
(defn mask? [a] (= :dtype/bool (:dtype a)))
(defn array? [a] (= :objtype/extension-array (:objtype a)))

(defn dataframe? [df] (= :objtype/dataframe (:objtype df)))
(defn series? [s] (= :objtype/series (:objtype s)))

(defn array-like? [a] ((some-fn sequential? array? ndarray?) a))

(defn datetimeindex? [idx] (= :objtype/datetimeindex (:objtype idx)))
(defn rangeindex? [idx] (= :objtype/rangeindex (:objtype idx)))
(defn any-index? [idx] (isa? bamboo-hierarchy (:objtype idx) :objtype/index))

(defn slice? [s] (= :objtype/slice (:objtype s)))
(defn array-expr? [e] (= :objtype/array-expr (:objtype e)))
