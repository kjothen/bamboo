(ns numcloj.array.item-manipulation
  (:require [numcloj.array.conversion :refer [item]]
            [numcloj.array-creation :refer [asarray empty*]]
            [numcloj.array-buffer :as b]
            [numcloj.functional :as f]
            [numcloj.traits :as traits]))

;;;; Array methods

;;; Item selection and manipulation

;; https://docs.scipy.org/doc/numpy/reference/generated/numpy.ndarray.put.html
(defn put
  "Replaces specified elements of an array with given values"
  [a ind v & {:keys [mode] :or {mode :raise}}]
  (let [_ind (asarray ind)
        _v (asarray v)]
    (asarray (b/assoc-index _ind _v a))))

;; https://docs.scipy.org/doc/numpy/reference/generated/numpy.ndarray.take.html#numpy.ndarray.take
(defn take*
  "Take elements from an array along an axis"
  [a indices & {:keys [axis out mode]}]
  (let [vf (f/vectorize (partial item a) :otypes [(:dtype a)])]
    (vf (asarray indices))))
    
;; https://docs.scipy.org/doc/numpy/reference/generated/numpy.ndarray.argsort.html#numpy.ndarray.argsort
(defn argsort
  "Returns the indices that would sort an array"
  [a & {:keys [axis kind order]
        :or {axis -1 kind traits/*sort-kind*}}]
  (let [len (:size a)
        indexed-a (empty* len :dtype :dtype/object)
        dst (empty* len :dtype :dtype/int64)
        comp-fn #(kind (b/get* %1 1) (b/get* %2 1))]
    (b/map-indexed-values #(:data (asarray [%1 %2])) a indexed-a)
    (b/sort-values (:data indexed-a) comp-fn)
    (b/map-values #(long (b/get* % 0)) indexed-a dst)
    (asarray dst)))
