(ns bamboo.dataframe
  (:refer-clojure :exclude [drop nth transpose])
  (:require [clojure.pprint :as pprint]
            [bamboo.utility :refer [array-zipmap condas-> in? to-vector]]
            [bamboo.array :as array]
            [bamboo.index :as index]
            [lang.core :refer [ndarray-expr]]
            [numcloj.core :as np]
            [numcloj.ndarray :as ndarray]))

(def ^:dynamic *head-rows* 5)

;;;; https://pandas.pydata.org/pandas-docs/stable/reference/frame.html

(defn- shape [as]
  [(first (:shape (first as))) (count as)])

(defn- index-positions [idx labels] (vals (select-keys (:loc idx) labels)))

;;; Constructor

;; https://pandas.pydata.org/pandas-docs/stable/reference/api/pandas.DataFrame.html#pandas.DataFrame
(defn dataframe
  "Two-dimensional size-mutable, potentially heterogeneous tabular data 
   structure with labeled axes (rows and columns). Arithmetic operations 
   align on both row and column labels. Can be thought of as a dict-like 
   container for Series objects. The primary bamboo data structure."           
  [data & {:keys [index columns dtype copy]
           :or {copy false}}]
  (let [arrays (mapv #(array/array % :dtype dtype :copy copy) 
                     (if (ndarray/ndarray? data) (ndarray/tolist data) data))
        shape (shape arrays)]
    {:dtype :dtype/dataframe
     :data (array/array arrays :dtype :dtype/object :copy false)
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
(defn- nth [a n] (ndarray/item (array/to-numpy a) n))
  
(defn iat
  "Access a single value for a row/column pair by integer position"
  [df index column]
  (nth (nth (:data df) column) index))

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
    
    (let [mvals (to-vector (or index (when (= 1 axis) labels)))
          nvals (to-vector (or columns (when (= 0 axis) labels)))
          m (when (seq mvals) (remove nil? (map #(index/get-loc (:index df) %) mvals)))
          n (when (seq nvals) (remove nil? (map #(index/get-loc (:columns df) %) nvals)))]
      (when (= errors :raise)
        (when-not (= (count mvals) (count m))
          (throw (ex-info (str "Not all index values can be found: " mvals)
                          {:type :KeyError})))
        (when-not (= (count nvals) (count n))
          (throw (ex-info (str "Not all column values can be found: " nvals)
                          {:type :KeyError}))))
      
      (println mvals m nvals n)
      (dataframe (condas-> (array/to-numpy (:data df)) $
                           (seq m) ((np/vectorize 
                                     #(np/delete (array/to-numpy %) m)) $)
                           (seq n) (np/delete $ n))
                 :index (condas-> (:index df) $
                                  (seq m) (index/drop $ mvals))
                 :columns (condas-> (:columns df) $
                                    (seq n) (index/drop $ nvals))
                 :copy true))))

;; https://pandas.pydata.org/pandas-docs/stable/reference/api/pandas.DataFrame.equals.html#pandas.DataFrame.equals
(defn equals
  "Test whether two objects contain the same elements"
  [df other]
  (and (= (:shape df) (:shape other))
       (index/equals (:columns df) (:columns other))
       (index/equals (:index df) (:index df))
       (reduce
        #(and %1 (let [a (nth (:data df) %2)
                       other-a (nth (:data other) %2)]
                   (and (= (:shape a) (:shape other-a))
                        (np/array-equal (array/to-numpy a)
                                        (array/to-numpy other-a)))))
        true
        (range (second (:shape df))))))

;;; Missing data handling
;;; Reshaping, sorting, transposing

;; https://pandas.pydata.org/pandas-docs/stable/reference/api/pandas.DataFrame.transpose.html#pandas.DataFrame.transpose
(defn transpose 
  "Transpose index and columns"
  [df & {:keys [copy] :or {copy false}}]
  (let [m (first (:shape df))
        n (second (:shape df))
        data (mapv (fn [i] (mapv (fn [j] (iat df i j)) (range n))) (range m))]
    (dataframe data
               :columns (:index df)
               :index (:columns df)
               :copy copy)))

;; https://pandas.pydata.org/pandas-docs/stable/reference/api/pandas.DataFrame.sort_values.html
(defn sort-values 
  "Sort by the values along either axis"
  [df by & {:keys [axis ascending inplace kind na-position]
            :or {axis 0 ascending true inplace false
                 sorting :quicksort na-position :last}}]
  (if (= 0 axis)
    (let [_by (to-vector by)
          positions (map #(index/get-loc (:columns df) %) _by)
          columns (mapv #(nth (:data df) %) positions)
          indices (if (= 1 (count columns))
                    (array/argsort (first columns))
                    (array/array (np/argsort (np/rec.fromarrays 
                                              (map array/to-numpy columns) 
                                              :names _by))))]
            (dataframe (array/to-numpy (:data df))
                       :columns (:columns df)
                       :index (index/index (array/take (index/array (:index df)) 
                                                       indices))
                       :copy false))))

;;; Combining / joining / merging
;;; Time series-related
;;; Plotting
;;; Serialization / IO / Conversion

;; https://pandas.pydata.org/pandas-docs/version/0.23/generated/pandas.DataFrame.from_dict.html#pandas.DataFrame.from_dict
(defn from-dict [data & {:keys [orient dtype columns]
                         :or {orient :columns}}]
  ; TODO: support constructing dataframe from rows to make csv easier
  
  )
