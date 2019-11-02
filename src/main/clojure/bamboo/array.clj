(ns bamboo.array
  (:require [numcloj.core :as np]
            [numcloj.ndarray :as ndarray]
            [bamboo.objtype :refer [array? mask? ndarray?]]))

;;;; An extension array for ndarrays

;;;; https://pandas.pydata.org/pandas-docs/version/0.24/reference/api/pandas.arrays.PandasArray.html
;;;; https://pandas.pydata.org/pandas-docs/version/0.23/generated/pandas.api.extensions.ExtensionArray.html

;;; Forward Declarations

(declare copy)
(declare to-numpy)

;;; Interface

(defn iter [a] (ndarray/tolist (to-numpy a)))
(defn item [a i] (ndarray/item (to-numpy a) i))

;;; Constructor

(defn- from-numpy [a]
  {:objtype :objtype/extension-array
   :dtype (:dtype a)
   :data a
   :size (:size a)
   :shape [(:size a) nil]})

;; https://pandas.pydata.org/pandas-docs/stable/reference/api/pandas.array.html
(defn array
  "Create an array"
  [values & {:keys [copy] :or {copy true}}]
  (cond
    (array? values) (if copy (bamboo.array/copy values) values)
    (ndarray? values) (let [a (if copy (np/copy values) values)]
                        (from-numpy a))
    :else (from-numpy (np/array values))))

;;; Attributes
(defn values [a] (:data a))

;;; Methods

;; https://pandas.pydata.org/pandas-docs/version/0.24/reference/api/pandas.arrays.PandasArray.argsort.html
(defn argsort
  "Return the indices that would sort this array" 
  [a & {:keys [ascending kind]
        :or {ascending true}}]
  (let [_a (np/argsort (to-numpy a))]
    (array (if ascending _a (np/flip a)) :copy false)))

;; https://pandas.pydata.org/pandas-docs/version/0.24/reference/api/pandas.arrays.PandasArray.copy.html#pandas.arrays.PandasArray.copy
(defn copy
  "Return a copy of the array"
  [a & {:keys [deep] :or {deep true}}]
  (array (np/copy (to-numpy a)) :copy false))

;; https://pandas.pydata.org/pandas-docs/version/0.24/reference/api/pandas.arrays.PandasArray.take.html
(defn take*
  "Take elements from an array"
  [a indices & {:keys [allow-fill fill-value]
                :or {allow-fill false}}]
  (if (int? indices)
    (array [(item a indices)])
    (let [_indices (to-numpy (array indices :copy false))]
      (array (np/take* (to-numpy a) (ndarray/tolist _indices))))))
            
;; https://pandas.pydata.org/pandas-docs/version/0.24/reference/api/pandas.arrays.PandasArray.to_numpy.html
(defn to-numpy
  "Convert the PandasArray to a numpy.ndarray"
  [a & {:keys [dtype copy] :or {copy false}}]
  (values (if copy (copy a) a)))
