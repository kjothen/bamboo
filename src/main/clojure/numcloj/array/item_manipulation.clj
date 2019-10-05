(ns numcloj.array.item-manipulation
  (:refer-clojure :exclude [take])
  (:require [numcloj.array.conversion :refer [item]]
            [numcloj.array-creation :refer [asarray]]
            [numcloj.array-buffer :as b]
            [numcloj.functional :as f]))

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
(defn take
  "Take elements from an array along an axis"
  [a indices & {:keys [axis out mode]}]
  (let [vf (f/vectorize (partial item a) :otypes [:dtype/object])]
    (vf (asarray indices))))
    
;; https://docs.scipy.org/doc/numpy/reference/generated/numpy.ndarray.argsort.html#numpy.ndarray.argsort
;; TODO - buffer implementation!!!
(defn argsort
  "Returns the indices that would sort an array"
  [a & {:keys [axis kind order]
        :or {axis -1}}]
  (let [indices (map first
                     (sort #(compare (second %1) (second %2))
                           (map-indexed vector (:data a))))]
    (asarray indices)))
