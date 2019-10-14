(ns numcloj.core
  (:require [numcloj.api.array-manipulation :as array-manipulation]
            [numcloj.api.counting :as counting]
            [numcloj.api.logic.array-contents :as logic.array-contents]
            [numcloj.api.logic.comparison :as logic.comparison]
            [numcloj.api.logic.truth :as logic.truth]
            [numcloj.api.searching :as searching]
            [numcloj.array-buffer :as b]
            [numcloj.array-creation :as array-creation]
            [numcloj.functional :as functional]
            [numcloj.ndarray :as ndarray]
            [numcloj.rec :as rec]))

;;;; TODO! For all functions that expect array-like, wrap these before sending to ndarray

;;;; https://docs.scipy.org/doc/numpy/reference/routines.html

;;; Array creation routines
;; Ones and zeros
(def empty* array-creation/empty*)
(def empty-like array-creation/empty-like)
(def full array-creation/full)
(def full-like array-creation/full-like)
(def ones array-creation/ones)
(def ones-like array-creation/ones-like)
(def zeros array-creation/zeros)
(def zeros-like array-creation/zeros-like)

;; From existing data
(def asarray array-creation/asarray)
(def array array-creation/array)
(def copy array-creation/copy)

;; Creating record arrays (numpy.rec)
(def recarray array-creation/recarray)
(def rec.fromarrays rec/fromarrays)

;;; Array manipulation routines
(def copyto array-manipulation/copyto)
(def delete array-manipulation/delete)

;;; Binary operations
;;; String operations
;;; C-Types Foreign Function Interface (numpy.ctypeslib)
;;; Datetime Support Functions
;;; Data type routines
;;; Optionally Scipy-accelerated routines (numpy.dual)
;;; Mathematical functions with automatic domain (numpy.emath)
;;; Floating point error handling
;;; Discrete Fourier Transform (numpy.fft)
;;; Financial functions
;;; Functional Programming
(def vectorize functional/vectorize)

;;; NumPy-specific help functions
;;; Indexing routines
(defn flatnonzero 
  "Return indices that are non-zero in the flattened version of a"
  [a]  
  (let [_a (asarray a)
        n (counting/count-nonzero _a)
        f (if (= :dtype/bool (:dtype _a))
            #(true? %2)
            #(not (zero? %2)))]
    (asarray (b/keep-indexed-indices f _a (empty* n :dtype :dtype/int64)))))

(defn put [a indices values & {:keys [mode]}]
  (ndarray/put (asarray a) indices :mode mode))

(defn take* [a indices & {:keys [axis out mode]}]  
  (ndarray/take* (asarray a) indices :axis axis :out out :mode mode))
  
;;; Input and output
;;; Linear algebra (numpy.linalg)
;;; Logic functions
;; Truth value testing
(def all logic.truth/all)
(def any logic.truth/any)

;; Array contents
(def isnan logic.array-contents/isnan)

;; Array type testing
;; Logical operations
;; Comparison
(def array-equal logic.comparison/array-equal)

;;; Masked array operations
;;; Mathematical functions
;;; Matrix library (numpy.matlib)
;;; Miscellaneous routines
;;; Padding Arrays
;;; Polynomials
;;; Random sampling (numpy.random)
;;; Set routines
;;; Sorting, searching and counting
;; Sorting
(defn argsort [a & {:keys [axis kind order] :or {axis -1}}]
  (let [_a (asarray a)]
    (if (= :dtype/record (:dtype _a))
      (rec/argsort _a :axis axis :kind kind :order order)
      (ndarray/argsort _a :axis axis :kind kind :order order))))

;; Searching
(def argmin searching/argmin)
(def argmax searching/argmax)

;; Counting
(def count-nonzero counting/count-nonzero)

;;; Statistics
;;; Test Support (numpy.testing)
;;; Window functions
