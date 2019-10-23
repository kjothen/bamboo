(ns bamboo.array
  (:require [numcloj.core :as np]
            [numcloj.ndarray :as ndarray]
            [bamboo.utility :refer [in?]]))

;;;; An extension array for ndarrays

;;;; https://pandas.pydata.org/pandas-docs/version/0.24/reference/api/pandas.arrays.PandasArray.html
;;;; https://pandas.pydata.org/pandas-docs/version/0.23/generated/pandas.api.extensions.ExtensionArray.html

(declare copy)
(declare to-numpy)

(defn array? [a] (= :objtype/extension-array (:objtype a)))
(defn ndarray? [a]
  (in? (:dtype a) #{:dtype/bool :dtype/float64 :dtype/int64 :dtype/object}))

(defn- from-numpy [a]
  {:objtype :objtype/extension-array
   :dtype :dtype/bamboo
   :data a
   :shape [(:size a) nil]})

;;; Interface

(defn from-sequence
  [data & {:keys [dtype copy] :or {copy true}}]
  (let [a (np/array data :dtype dtype)]
    (from-numpy a)))

(defn iter [a] (ndarray/tolist (to-numpy a)))
(defn item [a i] (ndarray/item (to-numpy a) i))

;;; Constructor
  
;; https://pandas.pydata.org/pandas-docs/stable/reference/api/pandas.array.html
(defn array
  "Create an array"
  [values & {:keys [copy] :or {copy true}}]
  (cond
    (array? values) (if copy (bamboo.array/copy values) values)
    (ndarray? values) (let [a (if copy (np/copy values) values)]
                        (from-numpy a))
    :else (from-sequence values :copy copy)))

;;; Attributes
(defn values [a] (:data a))

;;; Methods

;; https://pandas.pydata.org/pandas-docs/version/0.24/reference/api/pandas.arrays.PandasArray.argsort.html
(defn argsort
  "Return the indices that would sort this array" 
  [a & {:keys [ascending kind]
        :or {ascending true}}]
  (array (np/argsort (to-numpy a)) :copy false))

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
    (let [_indices (ndarray/tolist (to-numpy (array indices :copy false)))]
      (array (np/take* (to-numpy a) _indices)))))

;; https://pandas.pydata.org/pandas-docs/version/0.24/reference/api/pandas.arrays.PandasArray.to_numpy.html
(defn to-numpy
  "Convert the PandasArray to a numpy.ndarray"
  [a & {:keys [dtype copy] :or {copy false}}]
  (values (if copy (copy a) a)))
