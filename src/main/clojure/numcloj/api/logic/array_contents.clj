(ns numcloj.api.logic.array-contents
  (:require [numcloj.array-creation :refer [asarray]]
            [numcloj.functional :refer [vectorize]]
            [numcloj.utility :refer [nan?]]))

;;;; Logic functions

;;; Array contents

; https://docs.scipy.org/doc/numpy/reference/generated/numpy.isnan.html#numpy.isnan
(defn isnan
  "Test element-wise for NaN and return result as a boolean array"
  [a & {:keys [out where]}]
  (let [vf (vectorize nan? :otypes [:dtype/bool])]
    (vf (asarray a))))
