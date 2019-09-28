(ns bamboo.pprint
  (:refer-clojure :exclude [index])
  (:require [clojure.pprint]
            [bamboo.array :as array]
            [bamboo.dataframe :as dataframe]
            [bamboo.index :as index]
            [bamboo.utility :refer [array-zipmap]]))

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

(defn- data-rows
  [df row-range]
  (let [col-range (range (second (:shape df)))
        columns (map #(array/take (index/array (:columns df)) %) col-range)]
    (map (fn [row]
           (array-zipmap
            columns
            (map (fn [col] (dataframe/iat df row col)) col-range)))
         row-range)))

(defn- elipsis-row [df]
  (let [columns (index/array (:columns df))]
    (array-zipmap columns (repeat (count columns) "..."))))

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
