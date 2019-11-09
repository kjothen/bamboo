(ns bamboo.checked.core
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.test.alpha :as stest]
            [bamboo.checked.base :as base]
            [bamboo.checked.dataframe :as dataframe]
            [bamboo.checked.series :as series]
            [bamboo.core]))

;;;; https://pandas.pydata.org/pandas-docs/version/0.23/api.html

;;; Input/Output
;; Flat File
(s/def :read-csv/filepath-or-buffer string?)
(s/def :read-csv/sep char?)
(s/def :read-csv/quote-char char?)
(s/def :read-csv/header (s/or :int int? 
                              :int-list (s/coll-of int?)))
(s/def :read-csv/names ::base/array-like?)   
(s/def :read-csv/usecols char?)
(s/def :read-csv/prefix char?)
(s/def :read-csv/true-values ::base/array-like?)
(s/def :read-csv/false-values ::base/array-like?)
(s/def :read-csv/nrows nat-int?)
(s/def :read-csv/na-values ::base/array-like?)
(s/def :read-csv/keep-default-na boolean?)
(s/def :read-csv/na-filter ::base/array-like?)
(s/def :read-csv/skiprows nat-int?)
(s/def :read-csv/skipfooter nat-int?)
(s/def :read-csv/skip-blank-lines boolean?)
(s/def :read-csv/skipinitialspace boolean?)
(s/def :read-csv/comment* char?)

(s/fdef bamboo.core/read-csv
  :args (s/cat :filepath-or-buffer :read-csv/filepath-or-buffer
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
(s/def :date-range/start ::base/datetime-like?)
(s/def :date-range/end ::base/datetime-like?)
(s/def :date-range/periods nat-int?)
(s/def :date-range/tz string?)
(s/def :date-range/normalize boolean?)
(s/def :date-range/name* string?)
(s/def :date-range/closed boolean?)

(s/fdef bamboo.core/date-range
  :args (s/cat :kwargs (s/keys* :opt-un
                                [:date-range/start
                                 :date-range/end
                                 :date-range/periods
                                 :date-range/tz
                                 :date-range/normalize
                                 :date-range/name*
                                 :date-range/closed])))
(stest/instrument `bamboo.core/date-range)
(def date-range bamboo.core/date-range)

;;; Series
(def series series/series)

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

;;; Clojure Extensions
(def show bamboo.core/show)

(def logical-and bamboo.core/logical-and)
(def logical-or bamboo.core/logical-or)
(def equal bamboo.core/equal)
(def greater bamboo.core/greater)
(def greater-equal bamboo.core/greater-equal)
(def less bamboo.core/less)
(def less-equal bamboo.core/less-equal)
