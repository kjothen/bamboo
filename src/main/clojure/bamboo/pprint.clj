(ns bamboo.pprint
  (:refer-clojure :exclude [index])
  (:require [clojure.pprint]
            [bamboo.array :as array]
            [bamboo.dataframe :as dataframe]
            [bamboo.index :as index]
            [bamboo.utility :refer [array-zipmap]]
            [numcloj.ndarray :as ndarray]))

(def ^:dynamic *print-rows* 10)

(defn- head-range
  [df]
  (let [row-count (first (:shape df))]
    (if (> row-count *print-rows*)
      (range (long (/ *print-rows* 2)))
      (range (min row-count *print-rows*)))))

(defn- tail-range
  [df]
  (let [row-count (first (:shape df))]
    (if (> row-count *print-rows*)
      (range (- row-count (long (/ *print-rows* 2)))
             row-count))))

(defn- column-row
  [df]
  (cons (:name (:index df))
        (ndarray/tolist (index/to-numpy (:columns df)))))

(defn- data-rows
  [df row-range]
  (let [col-range (range (second (:shape df)))
        columns (column-row df)]
    (map (fn [row]
           (array-zipmap
            columns
            (cons
             (ndarray/item (index/to-numpy (:index df)) row)
             (map (fn [col] (dataframe/iat df row col)) col-range))))
         row-range)))

(defn- elipsis-row [df]
  (let [columns (column-row df)]
    (array-zipmap columns (repeat (inc (count columns)) "..."))))

(defmulti pprint :dtype)
(defmethod pprint :default [data] )
(defmethod pprint :dtype/dataframe [df]
  (let [head-rows (data-rows df (head-range df))
        tail-rows (data-rows df (tail-range df))]
    (if (empty? tail-rows)
      (clojure.pprint/print-table head-rows)
      (clojure.pprint/print-table (concat head-rows 
                                          [(elipsis-row df)] 
                                          tail-rows)))))

(defmethod pprint :dtype/datetimeindex [idx]
  (clojure.pprint/pprint (:data idx)))
