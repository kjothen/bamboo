(ns bamboo.checked.series
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.test.alpha :as stest]
            [bamboo.checked.base :as base]
            [bamboo.series]))

;;;; https://pandas.pydata.org/pandas-docs/stable/reference/series.html

;;; Constructor
(def series bamboo.series/series)

;;; Attributes 
(def array bamboo.series/array)
(def values bamboo.series/values)

;;; Conversion
(def copy bamboo.series/copy)
(def to-numpy bamboo.series/to-numpy)
(def to-list bamboo.series/to-list)

;;; Indexing, iteration
(def at bamboo.series/at)
(def at! bamboo.series/at!)
(def iat bamboo.series/iat)
(def iat! bamboo.series/iat!)
(def loc bamboo.series/loc)
(def items bamboo.series/items)
(def iteritems bamboo.series/iteritems)
(def iloc bamboo.series/iloc)
(def keys* bamboo.series/keys*)
(def item bamboo.series/item)

;;; Reindexing / selection / label manipulation
(def equals bamboo.series/equals)
(def take* bamboo.series/take*)
(def to-string bamboo.series/to-string)

;;; Clojure Extensions
(def show bamboo.series/show)
