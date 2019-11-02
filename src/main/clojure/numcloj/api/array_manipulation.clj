(ns numcloj.api.array-manipulation
  (:require [numcloj.api.counting :refer [count-nonzero]]
            [numcloj.array.item-manipulation :refer [put]]
            [numcloj.array-buffer :as b]
            [numcloj.array-creation :refer [asarray empty* empty-like 
                                            frombuffer  ones zeros]]))

;;;; Array manipulation routines
;;;; https://docs.scipy.org/doc/numpy/reference/routines.array-manipulation.html

;;; Basic operations
;; https://docs.scipy.org/doc/numpy/reference/generated/numpy.copyto.html
(defn copyto
  "Copies values from one array to another, 
   broadcasting as necessary"
  [dst src & {:keys [casting where]}]
  (let [_src (asarray src)]
    (if (some? where)
      (let [_where (asarray where)]
        (b/keep-indexed-values
         (fn [idx _] (= 1 (b/get* (:data _where) idx))) _src dst))
      (b/map-values identity _src dst))))

;;; Changing array shape
;;; Transpose-like operations
;;; Changing number of dimensions
;;; Changing kind of array
;;; Joining arrays
;;; Splitting arrays
;;; Tiling arrays
;;; Adding and removing elements

;; https://docs.scipy.org/doc/numpy/reference/generated/numpy.delete.html
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
        out (empty* (count-nonzero mask) :dtype (:dtype _a))]
    (copyto out _a :where mask)
    out))

;; https://docs.scipy.org/doc/numpy/reference/generated/numpy.resize.html
(defn resize
  "Return a new array with the specified shape"
  [a new-shape]
  (let [_a (asarray a)
        len (if (sequential? new-shape) (first new-shape) new-shape)]
    (frombuffer (b/copy (:data _a) len) :dtype (:dtype _a))))

;;; Rearranging elements

;; https://docs.scipy.org/doc/numpy/reference/generated/numpy.flip.html
(defn flip
  "Reverse the order of elements in an array along the given axis"
  [a & {:keys [axis]}]
  (let [_a (asarray a)
        r (empty-like _a)]
    (b/reverse-values _a r)))
