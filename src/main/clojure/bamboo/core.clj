(ns bamboo.core
  (:require [bamboo.array :as array]
            [bamboo.csv :as csv]
            [bamboo.dataframe :as dataframe]
            [bamboo.index :as index]
            [bamboo.date-range :as date-range]))

;;;; https://pandas.pydata.org/pandas-docs/version/0.23/api.html

;;; Input/Output
;; Flat File
(def read-csv csv/read-csv)

;;; General functions
;; Top-level dealing with datetimelike
(def date-range date-range/date-range)

;;; Series
;;; DataFrame
;; Constructor
(def dataframe dataframe/dataframe)

;;; Panel
;;; Index
(def index index/index)

;;; Numeric Index
(def rangeindex index/rangeindex)

;;; CategoricalIndex
;;; IntervalIndex
;;; MultiIndex
;;; Datetime Index
(def datetimeindex index/datetimeindex)

;;; TimedeltaIndex
;;; PeridoIndex
;;; Scalars
;;; Frequencies
;;; Window
;;; GroupBy
;;; Resampling
;;; Style
;;; Plotting
;;; General utility functions
;;; Extensions
(def array array/array)

  