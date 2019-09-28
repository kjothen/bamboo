(ns bamboo.array
  (:refer-clojure :exclude [take])
  (:require [numcloj.core :as np]
            [numcloj.ndarray :as ndarray]))

;;;; https://pandas.pydata.org/pandas-docs/version/0.24/reference/api/pandas.arrays.PandasArray.html
;;;; https://pandas.pydata.org/pandas-docs/version/0.23/generated/pandas.api.extensions.ExtensionArray.html#pandas.api.extensions.ExtensionArray

(defn- asarray
  "Convert the input to an array"
  [data & {:keys [dtype copy] :or {copy true}}]
  (let [a (np/array data :dtype dtype :copy copy)]
    (merge {:dtype :dtype/array
            :data a}
           (select-keys a [:nbytes :shape :ndim]))))

(defn array? [a] (and (map? a) (= (:dtype a) :dtype/array)))

;; https://pandas.pydata.org/pandas-docs/stable/reference/api/pandas.array.html
(defn array 
  "Create an array"
  [data & {:keys [dtype copy] :or {copy true}}]
  (if (array? data)
    (if copy 
      (array (np/array (:data data) :dtype dtype :copy true)) 
      data)
    (asarray data :dtype dtype :copy copy)))
  
;;; Attributes
(defn values [a] (:data a))

;;; Methods

;; https://pandas.pydata.org/pandas-docs/version/0.24/reference/api/pandas.arrays.PandasArray.argsort.html#pandas.arrays.PandasArray.argsort
(defn argsort [a & {:keys [ascending kind]
                    :or {ascending true}}]
  "Return the indices that would sort this array"
  (np/argsort (:data a) :axis 1 :kind kind))

;; https://pandas.pydata.org/pandas-docs/version/0.24/reference/api/pandas.arrays.PandasArray.take.html
(defn take
  "Take elements from an array"
  [a indices & {:keys [allow-fill fill-value]
                :or {allow-fill false}}]
  (if (sequential? indices)
    (array (np/take (:data a) indices))
    (ndarray/item (:data a) indices)))

;; https://pandas.pydata.org/pandas-docs/version/0.24/reference/api/pandas.arrays.PandasArray.to_numpy.html
(defn to-numpy
  "Convert the PandasArray to a numpy.ndarray"
  [a & {:keys [dtype copy] :or {copy false}}]
  (:data a))
