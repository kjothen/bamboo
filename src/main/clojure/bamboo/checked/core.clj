(ns bamboo.checked.core
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.test.alpha :as stest]
            [bamboo.core]
            [bamboo.checked.dataframe :as dataframe]
            [bamboo.utility :refer [scalar?]]))

;;;; https://pandas.pydata.org/pandas-docs/version/0.23/api.html

;;; Input/Output
;; Flat File
(s/def :read-csv/filepath-or-buffer string?)
(s/def :read-csv/sep char?)
(s/def :read-csv/quote-char char?)
(s/def :read-csv/header (s/or :int int? 
                              :int-list (s/coll-of int?)))
(s/def :read-csv/names (s/coll-of scalar?))   
(s/def :read-csv/usecols char?)
(s/def :read-csv/prefix char?)
(s/def :read-csv/true-values (s/coll-of scalar?))
(s/def :read-csv/false-values (s/coll-of scalar?))
(s/def :read-csv/nrows nat-int?)
(s/def :read-csv/na-values (s/coll-of string?))
(s/def :read-csv/keep-default-na boolean?)
(s/def :read-csv/na-filter (s/coll-of scalar?))
(s/def :read-csv/skiprows nat-int?)
(s/def :read-csv/skipfooter nat-int?)
(s/def :read-csv/skip-blank-lines boolean?)
(s/def :read-csv/skipinitialspace boolean?)
(s/def :read-csv/comment* char?)

(s/fdef bamboo.core/read-csv
  :args (s/cat :filepath-or-buffer string?
               :kwargs (s/keys* :opt-un
                                [:read-csv/sep
                                 :read-csv/quote-char
                                 :read-csv/header
                                 :read-csv/names
                                 :read-csv/usecols
                                 :read-csv/prefix
                                 :read-csv/true-values
                                 :read-csv/false-values
                                 :read-csv/nrows
                                 :read-csv/na-values
                                 :read-csv/keep-default-na
                                 :read-csv/na-filter
                                 :read-csv/skiprows
                                 :read-csv/skipfooter
                                 :read-csv/skip-blank-lines
                                 :read-csv/skipinitialspace
                                 :read-csv/comment*])))

(stest/instrument `bamboo.core/read-csv)
(def read-csv bamboo.core/read-csv)

;;; General functions
;; Top-level dealing with datetimelike
(def date-range bamboo.core/date-range)

;;; Series
;;; DataFrame
;; Constructor
(def dataframe dataframe/dataframe)

;;; Panel
;;; Index
(def index bamboo.core/index)

;;; Numeric Index
(def rangeindex bamboo.core/rangeindex)

;;; CategoricalIndex
;;; IntervalIndex
;;; MultiIndex
;;; Datetime Index
(def datetimeindex bamboo.core/datetimeindex)

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
(def array bamboo.core/array)
