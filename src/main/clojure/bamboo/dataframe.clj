(ns bamboo.dataframe
  (:require [clojure.string :as string]
            [taoensso.tufte :as tufte]
            [bamboo.utility :refer [array-zipmap condas-> in?
                                    scalar? to-vector]]
            [bamboo.array :as array]
            [bamboo.index :as index]
            [numcloj.core :as np]
            [numcloj.ndarray :as ndarray]))

(def ^:dynamic *head-rows* 5)

;;;; https://pandas.pydata.org/pandas-docs/stable/reference/frame.html

(defn- shape [as]
  [(first (:shape (first as))) (count as)])

(defn- copy-data [df]
  (array/array 
   (map array/copy (ndarray/tolist (array/to-numpy (:data df))))
   :dtype :dtype/object))

(def a2n array/to-numpy)
(def a2l (comp ndarray/tolist array/to-numpy))

;;; Constructor

;; https://pandas.pydata.org/pandas-docs/stable/reference/api/pandas.DataFrame.html#pandas.DataFrame
(defn dataframe
  "Two-dimensional size-mutable, potentially heterogeneous tabular data 
   structure with labeled axes (rows and columns). Arithmetic operations 
   align on both row and column labels. Can be thought of as a dict-like 
   container for Series objects. The primary bamboo data structure."
  [data & {:keys [index columns dtype copy]
           :or {copy false}}]
  (tufte/p
   :bamboo/dataframe.dataframe
   (let [arrays (mapv #(array/array % :dtype dtype :copy copy)
                      (if (ndarray/ndarray? data) (ndarray/tolist data) data))
         shape (shape arrays)]
     {:dtype :dtype/dataframe
      :dtypes (array/array (map #(:dtype (array/to-numpy %)) arrays) 
                           :dtype :dtype/object)
      :data (array/array arrays :dtype :dtype/object :copy false)
      :index (if (some? index)
               (index/index index :copy copy)
               (index/rangeindex (first shape)))
      :columns (if (some? columns)
                 (index/index columns :copy copy)
                 (index/rangeindex (second shape)))
      :shape shape})))

;;; Attributes and underlying data
;;; Conversion

;;; Indexing, iteration
(defn- nth* [a n] (ndarray/item (array/to-numpy a) n))

;; https://pandas.pydata.org/pandas-docs/stable/reference/api/pandas.DataFrame.iat.html  
(defn iat
  "Access a single value for a row/column pair by integer position"
  [df index column]
  (tufte/p :dataframe/iat (nth* (nth* (:data df) column) index)))

;; https://pandas.pydata.org/pandas-docs/stable/reference/api/pandas.DataFrame.iloc.html
(defn iloc
  "Purely integer-location based indexing for selection by position"
  ([df index] (array/array (map #(iat df index %) (range (second (:shape df))))))
  ([df index column] (iat df index column)))

;; https://pandas.pydata.org/pandas-docs/stable/reference/api/pandas.DataFrame.at.html
(defn at
  "Access a single value for a row/column label pair"
  [df index column]
  (iat (index/get-loc (:index df) index) (index/get-loc (:columns df) column)))

;; https://pandas.pydata.org/pandas-docs/stable/reference/api/pandas.DataFrame.loc.html
(defn loc
  "Access a group of rows and columns by label (s) or a boolean array"
  ([df index-label] (loc df index-label nil))
  ([df index-label column-label]
   (let [label-type-fn (fn [label index]
                         (cond
                           (scalar? label) :label
                           (and (map? label)
                                (= :objtype/slice (:objtype label))) :slice
                           (and (sequential? label)
                                (= (count label) (:size index))
                                (every? boolean? label)) :mask
                           (sequential? label) :labels
                           (fn? label) :callable))
         index-label-type (label-type-fn index-label (:index df))
         column-label-type (label-type-fn column-label (:columns df))]
     (when-not (or index-label-type column-label-type)
       (throw (ex-info (str "Must specify either a single label, "
                            "a list of labels, a slice, a boolean array "
                            "or callable function: " index-label column-label)
                       (:type :ValueError))))
     
     (let [m (case index-label-type
               :label (vector (index/get-loc (:index df) index-label))
               :labels (map #(index/get-loc (:index df) %) index-label)
               nil)
           mvals (when (some? m) (array/take* (index/array (:index df)) m))
           n (case column-label-type
               :label (vector (index/get-loc (:columns df) column-label))
               :labels (map #(index/get-loc (:columns df) %) column-label)
               nil)
           nvals (when (some? n) (array/take* (index/array (:columns df)) n))
           data (condas-> (:data df) $
                          (some? n) (array/take* $ n)
                          (some? m) (map #(array/take* % m)
                                         (ndarray/tolist (array/to-numpy $))))]
       (dataframe data
                  :index (if (some? mvals) (index/index mvals) (:index df))
                  :columns (if (some? nvals) (index/index nvals) (:columns df))
                  :copy false)))))

;; https://pandas.pydata.org/pandas-docs/stable/reference/api/pandas.DataFrame.itertuples.html
(defn itertuples
  "Iterate over DataFrame rows as namedtuples"
  [df & {:keys [index name*] :or {index true name* "Pandas"}}]
  (let [columns (a2l (index/to-native-types (:columns df)))
        records (np/rec.fromarrays (map a2n (a2l (:data df))) :names columns)]
    (a2l (array/array records))))

;;; Binary operator functions
;;; Function application, GroupBy & Window

;; https://pandas.pydata.org/pandas-docs/stable/reference/api/pandas.DataFrame.applymap.html
(defn applymap
  "This method applies a function that accepts and returns a scalar 
   to every element of a DataFrame" 
  [df func & {:keys [otype]}]
  ; TODO: don't supply otype, taint the first result to derive this
  (update-in df [:data]
             (fn [data]
               (array/array
                (map #(let [vfunc (np/vectorize 
                                   func 
                                   :otypes [(or otype (:dtype %))])
                            res (vfunc (array/to-numpy %))]
                        (array/array res))
                     (a2l data))
                :dtype :dtype/object))))
  

;;; Computations / Descriptive Stats
;;; Reindexing / Selection / label manipulation

;; https://pandas.pydata.org/pandas-docs/stable/reference/api/pandas.DataFrame.drop.html
(defn drop*
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
      (throw (ex-info (str "Need to specify at least one of "
                           "'labels', 'index' or 'columns'")
                      {:type :ValueError})))
    (when (and (some? labels) (not-every? nil? [columns index]))
      (throw (ex-info "Cannot specify both 'labels' and 'index'/'columns'"
                      {:type :ValueError})))

    (let [mvals (to-vector (or index (when (= 1 axis) labels)))
          nvals (to-vector (or columns (when (= 0 axis) labels)))
          index-loc-fn (fn [index locs]
                         (when (seq locs)
                           (keep #(index/get-loc index % :errors errors)
                                 locs)))
          m (index-loc-fn (:index df) mvals)
          n (index-loc-fn (:columns df) nvals)
          index-drop-fn (fn [index locs ilocs]
                          (if (seq ilocs)
                            (index/drop* index locs :errors errors)
                            (if (true? inplace) index (index/copy index))))]

      (dataframe (condas-> (array/to-numpy (if (true? inplace)
                                             (:data df)
                                             (copy-data df))) $
                           (seq m) ((np/vectorize
                                     #(np/delete (array/to-numpy %) m)) $)
                           (seq n) (np/delete $ n))
                 :index (index-drop-fn (:index df) mvals m)
                 :columns (index-drop-fn (:columns df) nvals n)
                 :copy false))))

;; https://pandas.pydata.org/pandas-docs/stable/reference/api/pandas.DataFrame.equals.html
(defn equals
  "Test whether two objects contain the same elements"
  [df other]
  (and (= (:shape df) (:shape other))
       (reduce
        #(and %1 (let [a (nth* (:data df) %2)
                       other-a (nth* (:data other) %2)]
                   (and (= (:shape a) (:shape other-a))
                        (np/array-equal (array/to-numpy a)
                                        (array/to-numpy other-a)))))
        true
        (range (second (:shape df))))))

;;; Missing data handling
;;; Reshaping, sorting, transposing

;; https://pandas.pydata.org/pandas-docs/stable/reference/api/pandas.DataFrame.transpose.html
(defn transpose* 
  "Transpose index and columns"
  [df & {:keys [copy] :or {copy false}}]
  (tufte/p
   :bamboo/dataframe.transpose*
   (let [m (first (:shape df))
         n (second (:shape df))
         data (mapv (fn [i] (mapv (fn [j] (iat df i j)) (range n))) (range m))]
     (dataframe data
                :columns (:index df)
                :index (:columns df)
                :copy copy))))

;; https://pandas.pydata.org/pandas-docs/stable/reference/api/pandas.DataFrame.sort_values.html
(defn sort-values
  "Sort by the values along either axis"
  [df by & {:keys [axis ascending inplace kind na-position]
            :or {axis 0 ascending true inplace false
                 sorting :quicksort na-position :last}}]
  (tufte/p
   :dataframe/sort-values
   (if (= 0 axis)
     (let [_by (to-vector by)
           positions (map #(index/get-loc (:columns df) %) _by)
           columns (mapv #(nth* (:data df) %) positions)
           indices (if (= 1 (count columns))
                     (array/argsort (first columns))
                     (array/array (np/argsort (np/rec.fromarrays
                                               (map array/to-numpy columns)
                                               :names _by))))
           index (index/index (array/take* (index/array (:index df))
                                           indices))
           data (map #(array/take* % indices)
                     (ndarray/tolist (array/to-numpy (:data df))))]
       (dataframe data
                  :columns (:columns df)
                  :index index
                  :copy false)))))

;;; Combining / joining / merging
;;; Time series-related
;;; Plotting
;;; Serialization / IO / Conversion

;; https://pandas.pydata.org/pandas-docs/stable/reference/api/pandas.DataFrame.to_string.html#pandas.DataFrame.to_string
(defn to-string
  [df & {:keys [columns col-space header index
                na-rep formatters float-format sparsify index-names justify
                max-rows max-cols show-dimensions decimal line-width]
         :or {show-dimensions false decimal \.}}]
  (let [columns-df (if (some? columns) (take* $ :columns columns) df)
        strings-df (applymap columns-df str :otype :dtype/object)
        _columns (a2l (index/array (:columns strings-df)))
        widths (as-> strings-df $
                 (applymap $ count)
                 (map #(long (np/amax (a2n %)))
                      (a2l (:data $)))
                 (map-indexed #(max (count (nth _columns %1)) %2) $))
        records (itertuples strings-df)
        linefn (fn [coll] (string/join 
                           " "
                           (map-indexed
                            #(format (str "%-" (nth widths %1) "s") %2)
                            coll)))
        headers (linefn _columns)
        body (map (comp linefn vals) records)]
    (str headers "\n" (string/join "\n" body))))
  