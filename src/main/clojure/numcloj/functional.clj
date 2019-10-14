(ns numcloj.functional
  (:require [numcloj.array-buffer :as b]
            [numcloj.array-creation :refer [asarray empty*]]))

;;;; Functional Programming

;; https://docs.scipy.org/doc/numpy/reference/generated/numpy.vectorize.html#numpy.vectorize
(defn vectorize
  "Generalized function class"
  [f & {:keys [otypes]}]
  (fn [a]
    (let [_a (asarray a)
          otype (or (first otypes) (:dtype a))
          o (empty* (:size _a) :dtype otype)]
      (b/map-values f _a o))))
