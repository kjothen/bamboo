(ns numcloj.array.conversion
  (:require [numcloj.array-buffer :as b]))

;;;; Array methods

;;; Array conversion

(defmulti downcast :dtype)
(defmethod downcast :default [a val] val)
(defmethod downcast :dtype/bool [a val] (boolean val))
(defmethod downcast :dtype/float64 [a val] (double val))
(defmethod downcast :dtype/int64 [a val] (long val))

;; https://docs.scipy.org/doc/numpy/reference/generated/numpy.ndarray.item.html#numpy.ndarray.item 
(defn item
  "Copy an element of an array to a standard scalar and return it"
  [a i]
  (b/get* (:data a) i))

;; https://docs.scipy.org/doc/numpy/reference/generated/numpy.ndarray.tolist.html#numpy.ndarray.tolist
(defn tolist
  "Return the array as an a.ndim-levels deep nested list of Clojure scalars"
  [a]
  (b/tolist (:data a)))

;; https://docs.scipy.org/doc/numpy/reference/generated/numpy.ndarray.itemset.html#numpy.ndarray.itemset
(defn itemset
  "Insert scalar into an array (scalar is cast to array’s dtype, if possible)"
  [a i x]
  (b/set* (:data a) i x))

;; https://docs.scipy.org/doc/numpy/reference/generated/numpy.ndarray.copy.html#numpy.ndarray.copy
(defn copy
  "Return a copy of the array"
  [a & {:keys [order]}]
  (update-in a [:data] b/copy))

;; https://docs.scipy.org/doc/numpy/reference/generated/numpy.ndarray.fill.html#numpy.ndarray.fill
(defn fill
  "Fill the array with a scalar value"
  [a val]
  (b/fill (:data a) (downcast a val)))
