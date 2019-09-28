(ns bamboo.dataframe
  (:refer-clojure :exclude [drop])
  (:require [bamboo.utility :refer [array-zipmap]]
            [bamboo.array :as array]
            [bamboo.index :as index]))

(def ^:dynamic *head-rows* 5)

;;;; https://pandas.pydata.org/pandas-docs/version/0.23/api.html#dataframe

;;; Constructor

;; https://pandas.pydata.org/pandas-docs/stable/reference/api/pandas.DataFrame.html#pandas.DataFrame
(defn dataframe
  "Two-dimensional size-mutable, potentially heterogeneous tabular data 
   structure with labeled axes (rows and columns). Arithmetic operations 
   align on both row and column labels. Can be thought of as a dict-like 
   container for Series objects. The primary bamboo data structure."           
  [data & {:keys [index columns dtype copy!]
           :or {copy! true}}]
  (let [arrays (mapv #(array/array % :dtype dtype :copy copy!) data)
        rowcount (first (:shape (first arrays)))
        colcount (count arrays)]
    {:dtype :dtype/dataframe
     :data arrays
     :index (or index (index/rangeindex rowcount))
     :columns (index/index columns)
     :shape [rowcount colcount]}))

;;; Attributes and underlying data
;;; Conversion

;;; Indexing, iteration
(defn iat
  "Access a single value for a row/column pair by integer position"
  ([df index column] (array/take (nth (:data df) column) index)))

(defn iloc
  "Purely integer-location based indexing for selection by position"
  ([df index] (array/array (map #(iat df index %) (range (second (:shape df))))))
  ([df index column] (iat df index column)))

;;; Binary operator functions
;;; Function application, GroupBy & Window
;;; Computations / Descriptive Stats
;;; Reindexing / Selection / label manipulation
(defn drop [df labels]
  "Drop specified labels from rows or columns"
  (let [cols (select-keys (:columns df) (set labels))
        col-idxs (set (vals cols))
        col-lbls (vec (keys cols))]
    (dataframe (vec (keep-indexed
                     #(when-not (contains? col-idxs %1) %2)
                     (:data df)))
               :index (:index df)
               :columns (keys (apply dissoc (:columns df) col-lbls))
               :copy! true)))

;;; Missing data handling
;;; Reshaping, sorting, transposing
(defn sort-values [by & {:keys [axis ascending inplace kind na-position]
                         :or {axis 0 ascending true inplace false 
                              sorting :quicksort na-position :last}}])

;;; Combining / joining / merging
;;; Time series-related
;;; Plotting
;;; Serialization / IO / Conversion

;; https://pandas.pydata.org/pandas-docs/version/0.23/generated/pandas.DataFrame.from_dict.html#pandas.DataFrame.from_dict
(defn from-dict [data & {:keys [orient dtype columns]
                         :or {orient :columns}}]
  ; TODO: support constructing dataframe from rows to make csv easier
  
  )
