(ns bamboo.checked.array
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.test.alpha :as stest]
            [bamboo.checked.base :as base]
            [bamboo.array]))

;;;; An extension array for ndarrays

;;;; https://pandas.pydata.org/pandas-docs/version/0.24/reference/api/pandas.arrays.PandasArray.html
;;;; https://pandas.pydata.org/pandas-docs/version/0.23/generated/pandas.api.extensions.ExtensionArray.html

;;; Interface
(def iter bamboo.array/iter)
(def item bamboo.array/iter)

;;; Constructor
(def array bamboo.array/array)

;;; Methods
(def argsort bamboo.array/argsort)
(def copy bamboo.array/copy)
(def take* bamboo.array/take*)
(def to-numpy bamboo.array/to-numpy)

;;; Clojure Extensions
(def show bamboo.array/show)
