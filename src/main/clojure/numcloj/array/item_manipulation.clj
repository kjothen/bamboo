(ns numcloj.array.item-manipulation
  (:refer-clojure :exclude [take])
  (:require [numcloj.array.conversion :as conversion]
            [numcloj.buffer :as buffer]
            [numcloj.creation :refer [array]]
            [numcloj.functional :refer [vectorize]]))

;;;; Array methods

;;; Item selection and manipulation

;; https://docs.scipy.org/doc/numpy/reference/generated/numpy.ndarray.take.html#numpy.ndarray.take
(defn take
  "Take elements from an array along an axis"
  [a indices & {:keys [axis out mode]}]
  (let [vf (vectorize (partial conversion/item a) :otypes [:dtype/object])]
    (vf (array indices))))
    
;; https://docs.scipy.org/doc/numpy/reference/generated/numpy.ndarray.argsort.html#numpy.ndarray.argsort
(defn argsort
  "Returns the indices that would sort an array"
  [a & {:keys [axis kind order]
        :or {axis -1}}]
  (let [indices (map first
                     (sort #(compare (second %1) (second %2))
                           (map-indexed vector (:data a))))]
    (array indices)))
