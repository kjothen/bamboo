(ns numcloj.api.array-manipulation
  (:refer-clojure :exclude [empty])
  (:require [numcloj.api.counting :refer [count-nonzero]]
            [numcloj.array.conversion :refer [item]]
            [numcloj.array.item-manipulation :refer [put]]
            [numcloj.array-creation :refer [asarray empty ones zeros]]
            [numcloj.array-buffer :as b]))

;;;; Array manipulation routines

;;; Basic operations
;;; Changing number of dimensions
;;; Changing array shape
;;; Transpose-like operations
;;; Changing kind of array
;;; Joining arrays
;;; Splitting arrays
;;; Tiling arrays
;;; Adding and removing elements

;; https://docs.scipy.org/doc/numpy/reference/generated/numpy.unique.html#numpy.unique
(defn unique
  "Find the unique elements of an array"
  [ar & {:keys [return-index return-inverse return-counts]
         :or {return-index false return-inverse false return-counts false}}]
  ; TODO
)
  

;; https://docs.scipy.org/doc/numpy/reference/generated/numpy.copyto.html
(defn copyto
  "Copies values from one array to another, 
   broadcasting as necessary"
  [dst src & {:keys [casting where]}]
  (let [_src (asarray src)]
    (if (some? where)
      (let [_where (asarray where)]
        (b/keep-indexed-values
         (fn [idx _] (= 1 (b/get (:data _where) idx))) _src dst))
      (b/copy _src dst))))

;; https://docs.scipy.org/doc/numpy/reference/generated/numpy.delete.html#numpy.delete
(defn delete
  "Return a new array with sub-arrays along an axis deleted. 
   For a one dimensional array, this returns those entries 
   not returned by arr [obj]"
  [a obj & [axis]]
  (let [_a (asarray a)
          _obj (asarray obj)
          mask (put (ones (:size _a) :dtype :dtype/int64)
                    _obj
                    (zeros (:size _a) :dtype :dtype/int64))
          out (empty (count-nonzero mask) :dtype (:dtype _a))]
      (copyto out _a :where mask)
      out))

;;; Rearranging elements
