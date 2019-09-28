(ns numcloj.ndarray
  (:refer-clojure :exclude [take])
  (:require [numcloj.array.conversion :as conversion]
            [numcloj.array.item-manipulation :as item-manipulation]))

;;;; https://docs.scipy.org/doc/numpy/reference/arrays.ndarray.html

;;; Array methods

;; Array conversion
(def item conversion/item)
(def itemset conversion/itemset)
(def fill conversion/fill)
(def copy conversion/copy)

;; Shape manipulation

;; Item selection and manipulation
(def take item-manipulation/take)
(def argsort item-manipulation/argsort)

;; Calculation

;;; Arithmetic, matrix multiplication, and comparison operations
;;; Special methods
