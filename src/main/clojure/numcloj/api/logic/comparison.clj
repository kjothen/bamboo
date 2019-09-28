(ns numcloj.api.logic.comparison
  (:require [numcloj.buffer :as buffer]
            [numcloj.creation :refer [array]]))

;;;; Logic functions

;;; Comparison

;; https://docs.scipy.org/doc/numpy/reference/generated/numpy.array_equal.html
(defn array-equal [a1 a2]
  (let [_a1 (array a1 :copy false)
        _a2 (array a2 :copy false)]
    (and (= (:dtype _a1) (:dtype _a2))
         (= (:shape _a1) (:shape _a2))
         (buffer/bequals (:data _a1) (:data _a2)))))
  