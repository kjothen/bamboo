(ns bamboo.core
  (:require [clojure.pprint]
            [bamboo.array :as array]
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

(def ^:dynamic *show-length* 100)
(def ^:dynamic *show-width* 80)

(defmulti show-numcloj :dtype)
(defmethod show-numcloj :default [a] 
  (binding [*print-level* 1
            *print-length* *show-length*]
    (clojure.pprint/pprint a)))

(defmulti show :objtype)
(defmethod show :default [m] 
  (binding [*print-level* 1 
            *print-length* *show-length*] 
    (clojure.pprint/pprint m)))

(defmethod show :objtype/dataframe [df] 
  (dataframe/show df *show-length*))

(defmethod show :objtype/datetimeindex [idx] 
  (index/show idx *show-length* *show-width*))

(defmethod show :objtype/rangeindex [idx]
  (index/show idx *show-length* *show-width*))

(defmethod show :objtype/index [idx]
  (index/show idx *show-length* *show-width*))

(defmethod show :objtype/extension-array [a]
  (show-numcloj (:data a)))