(ns bamboo.checked.dataframe
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.test.alpha :as stest]
            [bamboo.checked.base :as base]
            [bamboo.dataframe]
            [bamboo.objtype :refer [dataframe? ndarray? scalar?]]))

;; https://pandas.pydata.org/pandas-docs/stable/reference/api/pandas.DataFrame.html#pandas.DataFrame
(s/def :dataframe/data (s/and 
                        (s/every ::base/array-like?)
                        #(or (empty? %) 
                             (apply = (map count %)))))
(s/def :dataframe/index ::base/array-like?)
(s/def :dataframe/columns ::base/array-like?)
(s/def :dataframe/dtype
  #{:dtype/bool :dtype/float64 :dtype/int64 :dtype/object})
(s/def :dataframe/copy boolean?)

(s/fdef :bamboo.dataframe/dataframe
  :args (s/cat :data :dataframe/data
               :kwargs (s/keys* :opt-un
                                [:dataframe/index
                                 :dataframe/columns
                                 :dataframe/dtype
                                 :dataframe/copy])))
(stest/instrument `bamboo.dataframe/dataframe)
(def dataframe bamboo.dataframe/dataframe)

(def loc bamboo.dataframe/loc)

;; https://pandas.pydata.org/pandas-docs/stable/reference/api/pandas.DataFrame.sort_values.html
(s/def :sort-values/by (s/or :one scalar? :many ::base/array-like?))
(s/def :sort-values/axis (s/int-in 0 1))
(s/def :sort-values/ascending boolean?)
(s/def :sort-values/inplace boolean?)
(s/def :sort-values/kind #{:quicksort :mergesort :heapsort})
(s/def :sort-values/na-position #{:first :last})

(s/fdef :bamboo.dataframe/sort-values
  :args (s/cat :df dataframe?
               :by :sort-values/by
               :kwargs (s/keys* :opt-un
                                [:sort-values/axis
                                 :sort-values/ascending
                                 :sort-values/inplace
                                 :sort-values/kind
                                 :sort-values/na-position])))
(stest/instrument `bamboo.dataframe/sort-values)
(def sort-values bamboo.dataframe/sort-values)

;; https://pandas.pydata.org/pandas-docs/stable/reference/api/pandas.DataFrame.to_string.html
(s/def :to-string/buf #(instance? % java.io.Writer))
(s/def :to-string/columns ::base/array-like?)
(s/def :to-string/col-space nat-int?)
(s/def :to-string/header boolean?)
(s/def :to-string/index boolean?)
(s/def :to-string/na-rep string?)
(s/def :to-string/formatters (s/or :coll-fns (s/coll-of fn?) 
                                   :map-fns (s/map-of scalar? fn?)))
(s/def :to-string/float-format fn?)
(s/def :to-string/sparsify boolean?)
(s/def :to-string/index-names boolean?)
(s/def :to-string/justify #{:left :right :center :justify :justify-all
                            :start :end :inherit :match-parent :initial})
(s/def :to-string/max-rows nat-int?)
(s/def :to-string/max-cols nat-int?)
(s/def :to-string/show-dimensions boolean?)
(s/def :to-string/decimal char?)
(s/def :to-string/line-width nat-int?)

(s/fdef bamboo.dataframe/to-string
  :args (s/cat :df dataframe?
               :kwargs (s/keys* :opt-un
                                [:to-string/buf
                                 :to-string/columns
                                 :to-string/col-space
                                 :to-string/header
                                 :to-string/index
                                 :to-string/na-rep
                                 :to-string/formatters
                                 :to-string/float-format
                                 :to-string/sparsify
                                 :to-string/index-names
                                 :to-string/justify
                                 :to-string/max-rows
                                 :to-string/max-cols
                                 :to-string/show-dimensions
                                 :to-string/decimal
                                 :to-string/line-width])))
(stest/instrument `bamboo.dataframe/to-string)
(def to-string bamboo.dataframe/to-string)

;;; Clojure Extensions
(s/def :expr/e (s/or :one scalar? :many ndarray?))
(s/fdef bamboo.dataframe/expr
  :args (s/cat :df dataframe?
               :e :expr/e))
(stest/instrument `bamboo.dataframe/expr)
(def expr bamboo.dataframe/expr)
