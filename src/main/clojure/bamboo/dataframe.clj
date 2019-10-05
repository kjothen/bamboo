(ns bamboo.dataframe
  (:refer-clojure :exclude [drop])
  (:require [bamboo.utility :refer [array-zipmap in? to-vector]]
            [bamboo.array :as array]
            [bamboo.index :as index]
            [lang.core :refer [ndarray-expr]]
            [numcloj.core :as np]))

(def ^:dynamic *head-rows* 5)

;;;; https://pandas.pydata.org/pandas-docs/stable/reference/api/pandas.DataFrame.html

(defn- shape [as] [(first (:shape (first as))) (count as)])

;;; Constructor

;; https://pandas.pydata.org/pandas-docs/stable/reference/api/pandas.DataFrame.html#pandas.DataFrame
(defn dataframe
  "Two-dimensional size-mutable, potentially heterogeneous tabular data 
   structure with labeled axes (rows and columns). Arithmetic operations 
   align on both row and column labels. Can be thought of as a dict-like 
   container for Series objects. The primary bamboo data structure."           
  [data & {:keys [index columns dtype copy]
           :or {copy false}}]
  (let [arrays (mapv #(array/array % :dtype dtype :copy copy) data)
        shape (shape arrays)]
    {:dtype :dtype/dataframe
     :data arrays
     :index (if (some? index) 
              (index/index index :copy copy)
              (index/rangeindex (first shape)))
     :columns (if (some? columns) 
                (index/index columns :copy copy) 
                (index/rangeindex (second shape)))
     :shape shape}))

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

;; https://pandas.pydata.org/pandas-docs/stable/reference/api/pandas.DataFrame.drop.html#pandas.DataFrame.drop
(defn drop
  "Drop specified labels (or columns and indices) from rows or columns"
  {:arglists '([df labels? & {:keys [index columns axis level inplace errors]
                              :or {axis 0 inplace false errors :raise}}])}
  [df-and-labels & args]
  ; more than one variadic overload - labels are optional
  (let [[df labels {:keys [index columns axis level inplace errors]
                    :or {axis 0 inplace false errors :raise}}]
        (if (even? (count args))
          [df-and-labels nil args]
          [df-and-labels (first args) (rest args)])]
    (when (every? nil? [labels columns index])
      (throw (ex-info "Need to specify at least one of 'labels', 'index' or 'columns'"
                      {:type :ValueError})))
    (when (and (some? labels) (not-every? nil? [columns index]))
      (throw (ex-info "Cannot specify both 'labels' and 'index'/'columns'"
                      {:type :ValueError})))
    ; (if (some? labels)
    ;   (if (= 0 axis)
    ;     (if (true? inplace)
    ;       (index/drop index labels :errors errors)
    ;       (index/drop index labels :errors errors))
    ;     (index/drop column labels :errors errors))
    
    ;; TODO - finish this properly now that delete works in numcloj!!!
    (case axis
      0 (let [_labels (to-vector labels)
              indices (np/flatnonzero (ndarray-expr (index/to-numpy (:columns df))
                                                    #(in? % _labels)
                                                    :dtype :dtype/bool))]
          (dataframe (vec (keep-indexed
                           #(when-not (in? %1 (:data indices)) %2)
                           (:data df)))
                     :index (:index df)
                     :columns (reduce #(index/delete %1 %2) (:columns df) _labels)
                     :copy true))
      df)))

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
