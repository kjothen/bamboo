(ns bamboo.array
  (:refer-clojure :exclude [take])
  (:require [taoensso.tufte :as tufte]
            [numcloj.core :as np]
            [numcloj.ndarray :as ndarray]))

;;;; https://pandas.pydata.org/pandas-docs/version/0.24/reference/api/pandas.arrays.PandasArray.html
;;;; https://pandas.pydata.org/pandas-docs/version/0.23/generated/pandas.api.extensions.ExtensionArray.html#pandas.api.extensions.ExtensionArray

(declare to-numpy)

(defn- asarray
  "Convert the input to an array using numcloj"
  [data & {:keys [dtype copy] :or {copy true}}]
  (let [a (np/array data :dtype dtype :copy copy)]
    (merge {:dtype :dtype/array
            :data a}
           (select-keys a [:nbytes :shape :ndim]))))

(defn- array? 
  "Return true if this is a bamboo array"
  [a] 
  (and (map? a) (= (:dtype a) :dtype/array)))

;; https://pandas.pydata.org/pandas-docs/stable/reference/api/pandas.array.html
(defn array 
  "Create an array"
  [data & {:keys [dtype copy] :or {copy true}}]
  (tufte/p
   :bamboo/array.array
   (if (array? data)
     (if copy 
       (array (np/array (:data data) :dtype dtype :copy true)) 
       data)
     (asarray data :dtype dtype :copy copy))))

;;; Attributes
(defn values [a] (:data a))

;;; Methods

;; https://pandas.pydata.org/pandas-docs/version/0.24/reference/api/pandas.arrays.PandasArray.argsort.html#pandas.arrays.PandasArray.argsort
(defn argsort
  "Return the indices that would sort this array" 
  [a & {:keys [ascending kind]
        :or {ascending true}}]
  (asarray (np/argsort (:data a))))

;; https://pandas.pydata.org/pandas-docs/version/0.24/reference/api/pandas.arrays.PandasArray.take.html
(defn take
  "Take elements from an array"
  [a indices & {:keys [allow-fill fill-value]
                :or {allow-fill false}}]
  (if (array? indices)
    (array (np/take (to-numpy a) (to-numpy indices)))
    (if (sequential? indices)
      (array (np/take (to-numpy a) indices))
      (array [(ndarray/item (to-numpy a) indices)]))))

;; https://pandas.pydata.org/pandas-docs/version/0.24/reference/api/pandas.arrays.PandasArray.to_numpy.html
(defn to-numpy
  "Convert the PandasArray to a numpy.ndarray"
  [a & {:keys [dtype copy] :or {copy false}}]
  (:data a))
