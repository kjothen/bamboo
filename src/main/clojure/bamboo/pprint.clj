(ns bamboo.pprint
  (:refer-clojure :exclude [index])
  (:require [clojure.pprint :as pprint]
            [io.aviso.ansi :as ansi]
            [bamboo.dataframe :as dataframe]
            [bamboo.index :as index]
            [bamboo.utility :refer [array-zipmap]]
            [numcloj.ndarray :as ndarray]))

(def ^:dynamic *print-rows* 60)

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

(defn print-table
  "Prints a collection of maps in a textual table. Prints table headings
   ks, and then a line of output for each row, corresponding to the keys
   in ks. If ks are not specified, use the keys of the first item in rows."
  {:added "1.3"}
  ([ks rows]
   (when (seq rows)
     (let [widths (map
                   (fn [k]
                     (apply max (count (str k)) (map #(count (str (get % k))) rows)))
                   ks)
           spacers (map #(apply str (repeat % "-")) widths)
           fmts (map #(str "%" % "s") widths)
           fmt-row (fn [leader divider trailer row
                        & {:keys [sep-ansi row-ansi first-ansi]
                           :or {sep-ansi identity row-ansi identity first-ansi identity}}]
                     (str (sep-ansi leader)
                          (apply str (interpose
                                      (sep-ansi divider)
                                      (concat
                                       (let [col (get row (first ks))
                                             fmt (first fmts)]
                                         (vector (first-ansi (format fmt (str col)))))
                                       (for [[col fmt]
                                             (map vector (map #(get row %) (rest ks)) (rest fmts))]
                                         (row-ansi (format fmt (str col)))))))
                          (sep-ansi trailer)))]
       (println)
       (println (fmt-row "| " " | " " |" (zipmap ks ks) 
                         :row-ansi ansi/bold-white))
       
       (println (fmt-row "|-" "-+-" "-|" (zipmap ks spacers)))
       
       (let [band (atom true)]
         (doseq [row rows]
           (if @band
             (println (fmt-row "| " " | " " |" row
                               :row-ansi ansi/white
                               :first-ansi ansi/bold-white))
             (println (fmt-row "| " " | " " |" row
                               :row-ansi ansi/white
                               :first-ansi ansi/bold-white)))
           (swap! band not)           
           )))))
  ([rows] (print-table (keys (first rows)) rows)))

(defmulti pprint :dtype)
(defmethod pprint :default [data] (clojure.pprint/pprint data))
(defmethod pprint :dtype/dataframe [df]
  (let [head-rows (data-rows df (head-range df))
        tail-rows (data-rows df (tail-range df))]
    (if (empty? tail-rows)
      (print-table head-rows)
      (print-table (concat head-rows
                           [(elipsis-row df)]
                           tail-rows)))))

(defmethod pprint :dtype/datetimeindex [idx]
  (clojure.pprint/pprint (:data idx)))
