(ns numcloj.functional
  (:refer-clojure :exclude [empty])
  (:require [numcloj.creation :as creation :refer [empty]]))

;;;; Functional Programming

;;; helpers
(defmacro amap-into!
  [to from val expr]
  `(let [l# (alength ~to)]
     (loop [idx# 0]
       (if (< idx# l#)
         (do
           (let [~val (aget ~from idx#)]
             (aset ~to idx# ~expr)
             (recur (unchecked-inc idx#))))
         ~to))))

;; https://docs.scipy.org/doc/numpy/reference/generated/numpy.vectorize.html#numpy.vectorize
(defn vectorize
  "Generalized function class"
  [f & {:keys [otypes]}]
  #(let [size (:size %)
         dtype (:dtype %)
         data (:data %)
         otype (or (first otypes) (:dtype %))
         o (empty size :dtype otype)
         odata (:data o)]
     (case [dtype otype]
       [:dtype/bool :dtype/bool] (amap-into! ^booleans odata ^booleans data val ^Boolean (f val))
       [:dtype/bool :dtype/float64] (amap-into! ^doubles odata ^booleans data val ^Double (f val))
       [:dtype/bool :dtype/int64] (amap-into! ^longs odata ^booleans data val ^Long (f val))
       [:dtype/bool :dtype/object] (amap-into! odata ^booleans data val (f val))

       [:dtype/float64 :dtype/bool] (amap-into! ^booleans odata ^doubles data val ^Boolean (f val))
       [:dtype/float64 :dtype/float64] (amap-into! ^doubles odata ^doubles data val ^Double (f val))
       [:dtype/float64 :dtype/int64] (amap-into! ^longs odata ^doubles data val ^Long (f val))
       [:dtype/float64 :dtype/object] (amap-into! odata ^doubles data val (f val))

       [:dtype/int64 :dtype/bool] (amap-into! ^booleans odata ^longs data val ^Boolean (f val))
       [:dtype/int64 :dtype/float64] (amap-into! ^doubles odata ^longs data val ^Double (f val))
       [:dtype/int64 :dtype/int64] (amap-into! ^longs odata ^longs data val ^Long (f val))
       [:dtype/int64 :dtype/object] (amap-into! odata ^longs data val (f val))

       [:dtype/object :dtype/bool] (amap-into! ^booleans odata data val ^Boolean (f val))
       [:dtype/object :dtype/float64] (amap-into! ^doubles odata data val ^Double (f val))
       [:dtype/object :dtype/int64] (amap-into! ^longs odata data val ^Long (f val))
       [:dtype/object :dtype/object] (amap-into! odata data val (f val))

       (amap-into! odata data val (f val)))
     o))
