(ns bamboo.core
  (:require [clojure.pprint]
            [bamboo.array :as array]
            [bamboo.csv :as csv]
            [bamboo.dataframe :as dataframe]
            [bamboo.date-range :as date-range]
            [bamboo.index :as index]
            [bamboo.series :as series]
            [numcloj.core :as np]))

;;;; https://pandas.pydata.org/pandas-docs/version/0.23/api.html

;;; Input/Output
;; Flat File
(def read-csv csv/read-csv)

;;; General functions
;; Top-level dealing with datetimelike
(def date-range date-range/date-range)

;;; Series
;; Constructor
(def series series/series)

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

;;; Clojure Extensions
(def ^:dynamic *show-length* 100)
(def ^:dynamic *show-width* 80)

(declare show-numcloj)

(defmulti show (fn [coll & _] (:objtype coll)))
(defmethod show :default [m & _] 
  (binding [*print-level* 1 
            *print-length* *show-length*] 
    (clojure.pprint/pprint m)))

(defmethod show :objtype/dataframe [df & args] 
  (apply (partial dataframe/show df) 
         (list* :max-rows *show-length* args)))

(defmethod show :objtype/series [series & args]
  (apply (partial series/show series)
         (list* :max-rows *show-length* args)))

(defmethod show :objtype/datetimeindex [idx & args] 
  (apply (partial index/show idx) 
         (list* :max-rows *show-length* :max-width *show-width* args)))

(defmethod show :objtype/rangeindex [idx & _]
  (index/show idx))

(defmethod show :objtype/index [idx & args]
  (apply (partial index/show idx)
         (list* :max-rows *show-length* :max-width *show-width* args)))

(defmethod show :objtype/extension-array [a & args]
  (apply (partial array/show a)
         (list* :max-rows *show-length* :max-width *show-width* args)))

;; Bamboo Array Comparison
(defmulti to-numpy :objtype)
(defmethod to-numpy :default [x] x)
(defmethod to-numpy :objtype/extension-array [a] (array/to-numpy a))
(defmethod to-numpy :objtype/series [a] (series/to-numpy a))

(defn logical-and [x y] (np/logical-and (to-numpy x) (to-numpy y)))
(defn logical-or [x y] (np/logical-or (to-numpy x) (to-numpy y)))
(defn equal [x y] (np/equal (to-numpy x) (to-numpy y)))
(defn greater [x y] (np/greater (to-numpy x) (to-numpy y)))
(defn greater-equal [x y] (np/greater-equal (to-numpy x) (to-numpy y)))
(defn less [x y] (np/less (to-numpy x) (to-numpy y)))
(defn less-equal [x y] (np/less-equal (to-numpy x) (to-numpy y)))
