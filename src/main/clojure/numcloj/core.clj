(ns numcloj.core
  (:refer-clojure :exclude [empty take])
  (:require [numcloj.api.logic.array-contents :as logic.array-contents]
            [numcloj.api.logic.comparison :as logic.comparison]
            [numcloj.api.logic.truth :as logic.truth]
            [numcloj.api.searching :as searching]
            [numcloj.creation :as creation]
            [numcloj.functional :as functional]
            [numcloj.ndarray :as ndarray]))

;;;; TODO! For all functions that expect array-like, wrap these before sending to ndarray

;;;; https://docs.scipy.org/doc/numpy/reference/routines.html

;;; Array creation routines
;; Ones and zeros
(def empty creation/empty)
(def empty-like creation/empty-like)
(def full creation/full)
(def full-like creation/full-like)
(def ones creation/ones)
(def ones-like creation/ones-like)
(def zeros creation/zeros)
(def zeros-like creation/zeros-like)

;; From existing data
(def asarray creation/asarray)
(def array creation/array)
(def copy creation/copy)

;;; Array manipulation routines
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
(defn take [a indices & {:keys [axis out mode]}]  
  (ndarray/take (array a) indices :axis axis :out out :mode mode))
  
;;; Input and output
;;; Linear algebra (numpy.linalg)
;;; Logic functions
;; Truth value testing
(def all logic.truth/all)
(def any logic.truth/any)

;; Array contents
(def isnan logic.array-contents/isnan)

;; Comparison
(def array-equal logic.comparison/array-equal)

;; Array type testing
;; Logical operations
;; Comparison

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
  (ndarray/argsort (array a) :axis axis :kind kind :order order))

;; Searching
(def argmin searching/argmin)
(def argmax searching/argmax)

;; Counting

;;; Statistics
;;; Test Support (numpy.testing)
;;; Window functions
