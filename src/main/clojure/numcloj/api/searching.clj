(ns numcloj.api.searching
  (:require [numcloj.dtype :as dtype]))

(defmulti argmax
  "Return indices of the maximum values along the given axis"
  :dtype :hierarchy #'dtype/numcloj-hierarchy)
(defmethod argmax :default [a]
  (throw (ex-info (str "argmax not allowed for this dtype: " (:dtype a))
                  {:type :TypeError})))
(defmethod argmax :dtype/number [a]
  (first (apply max-key second (map-indexed vector (:data a)))))

(defmulti argmin
  "Return index of the first minimum value"
  :dtype :hierarchy #'dtype/numcloj-hierarchy)
(defmethod argmin :default [a]
  (throw (ex-info (str "argmin not allowed for this dtype: " (:dtype a))
                  {:type :TypeError})))
(defmethod argmin :dtype/number [a]
  (first (apply min-key second (map-indexed vector (:data a)))))