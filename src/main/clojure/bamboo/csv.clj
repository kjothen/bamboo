(ns bamboo.csv
  (:require [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [clojure.string :as string]
            [taoensso.timbre :as timbre :refer [debug]]
            [bamboo.dataframe :as dataframe]
            [bamboo.utility :refer [parse-long parse-double parse-bool]])
  (:refer-clojure :exclude [read-string]))

(defn parse-string [s] (if (empty? s) nil s))

(defn- read-string [s]
  (let [s' (string/trim s)]
    (or (parse-long s') (parse-double s') (parse-bool s') (parse-string s'))))

(defn- read-rows [reader]
  (->> (csv/read-csv reader)
       (map #(map read-string %))))

(defn- col-names [rows header names]
  (cond
    (some? names) names
    (int? header) (nth rows header)
    :else (let [n (reduce max 0 (map count rows))]
            (map #(str " col" %) (range n)))))

(defn- row-data [rows header names]
  (cond
    (some? names) (nthrest rows ((fnil inc -1) header))
    (int? header) (nthrest rows (inc header))
    :else rows))

(defn- pad-row-data [row-data max-col-count]
  (map (fn [row]
         (let [col-count (count row)]
           (if (< col-count max-col-count)
             (concat row (repeat (- max-col-count col-count) nil))
             row)))
       row-data))

(defn read-csv
  "Construct a dataframe from a CSV file"
  [f & {:keys [header names]
        :or {header 0 names nil}}]
  (with-open [reader (io/reader f)]
    (let [rows (read-rows reader)
          columns (col-names rows header names)
          row-data (row-data rows header names)]
      (dataframe/dataframe
       (apply map vector (pad-row-data row-data (count columns)))
       :columns columns))))
