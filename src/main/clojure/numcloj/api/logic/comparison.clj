(ns numcloj.api.logic.comparison
  (:require [numcloj.array-buffer :as b]
            [numcloj.array-creation :refer [asarray]]))

;;;; Logic functions

;;; Comparison

;; https://docs.scipy.org/doc/numpy/reference/generated/numpy.array_equal.html
(defn array-equal [a1 a2]
  (let [_a1 (asarray a1)
        _a2 (asarray a2)]
    (and (= (:dtype _a1) (:dtype _a2))
         (= (:shape _a1) (:shape _a2))
         (b/equals (:data _a1) (:data _a2)))))
  