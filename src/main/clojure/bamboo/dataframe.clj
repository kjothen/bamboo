(ns bamboo.dataframe
  (:require [clojure.string :as string]
            [io.aviso.ansi :as ansi]
            [taoensso.tufte :as tufte]
            [bamboo.lang :refer [slice?]]
            [bamboo.utility :refer [array-zipmap condas-> dots in?
                                    scalar? spaces to-vector]]
            [bamboo.array :as array]
            [bamboo.index :as index]
            [numcloj.core :as np]
            [numcloj.ndarray :as ndarray]))

;;;; https://pandas.pydata.org/pandas-docs/stable/reference/frame.html

(def a2n array/to-numpy)
(def a2l (comp ndarray/tolist array/to-numpy))

(defn- copy-data [df]
  (array/array 
   (mapv array/copy (array/iter (:data df))) 
   :dtype :dtype/object :copy false))

(defn- asarrays 
  [data & {:keys [copy] :or {copy false}}]
  (array/array
   (cond
     (array/ndarray? data) (mapv #(array/array % :copy copy)
                                 (ndarray/tolist data))
     (array/array? data) (mapv #(array/array % :copy copy)
                               (array/iter data))
     :else (mapv #(array/array % :copy copy) data))
   :dtype :dtype/object :copy false))

;;; Forward Declarations
(declare take*)

;;; Constructor

;; https://pandas.pydata.org/pandas-docs/stable/reference/api/pandas.DataFrame.html#pandas.DataFrame
(defn dataframe
  "Two-dimensional size-mutable, potentially heterogeneous tabular data 
   structure with labeled axes (rows and columns). Arithmetic operations 
   align on both row and column labels. Can be thought of as a dict-like 
   container for Series objects. The primary pandas data structure."
  [data & {:keys [index columns dtype copy]
           :or {copy false}}]
  (tufte/p
   :bamboo/dataframe.dataframe
   (let [arrays (asarrays data :copy copy)
         shape [(first (:shape (array/item arrays 0))) (first (:shape arrays))]]
     {:objtype :objtype/dataframe
      :data arrays
      :dtypes (array/array (map #(:dtype (a2n %)) (array/iter arrays))
                           :dtype :dtype/object)
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

;; https://pandas.pydata.org/pandas-docs/stable/reference/api/pandas.DataFrame.iat.html  
(defn iat
  "Access a single value for a row/column pair by integer position"
  [df index column]
  (array/item (array/item (:data df) column) index))

;; https://pandas.pydata.org/pandas-docs/stable/reference/api/pandas.DataFrame.iloc.html
(defn iloc
  "Purely integer-location based indexing for selection by position"
  ([df index] 
   (cond
     (scalar? index) (array/array (map #(iat df index %)
                                       (range (second (:shape df)))))
     (array/ndarray? index) (let [indices (keep-indexed #(when (true? %2) %1)
                                                        (ndarray/tolist index))]
                              (take* df indices))))
  ([df index column] (iat df index column)))

;; https://pandas.pydata.org/pandas-docs/stable/reference/api/pandas.DataFrame.at.html
(defn at
  "Access a single value for a row/column label pair"
  [df index column]
  (iat (index/get-loc (:index df) index) 
       (index/get-loc (:columns df) column)))

;; https://pandas.pydata.org/pandas-docs/stable/reference/api/pandas.DataFrame.loc.html
(defn loc
  "Access a group of rows and columns by label (s) or a boolean array"
  ([df index-label] (loc df index-label nil))
  ([df index-label column-label]
   (let [label-type-fn (fn [label index]
                         (cond
                           (scalar? label) :label
                           (array/mask? label) :mask
                           (array/ndarray? label) :labels
                           (fn? label) :callable
                           (slice? label) :slice
                           (sequential? label) :labels))
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
               :slice (let [[start end]
                            (index/slice-locs (:index df)
                                              :start (:start index-label)
                                              :end (:end index-label))]
                        (range start (inc end)))
               :mask (ndarray/tolist (np/flatnonzero index-label))
               nil)         
           mvals (when (some? m) (array/take* (index/array (:index df)) m))
           n (case column-label-type
               :label (vector (index/get-loc (:columns df) column-label))
               :labels (map #(index/get-loc (:columns df) %) column-label)
               :slice (let [[start end]
                            (index/slice-locs (:columns df)
                                              :start (:start column-label)
                                              :end (:end column-label))]
                        (range start (inc end)))
               :mask (ndarray/tolist (np/flatnonzero column-label))
               nil)
           _ (println column-label-type n)
           nvals (when (some? n) (array/take* (index/array (:columns df)) n))
           data (condas-> (:data df) $
                          (some? n) (array/take* $ n)
                          (some? m) (map #(array/take* % m) (array/iter $)))]
       (dataframe data
                  :index (if (some? mvals) (index/index mvals) (:index df))
                  :columns (if (some? nvals) (index/index nvals) (:columns df))
                  :copy false)))))

;; https://pandas.pydata.org/pandas-docs/stable/reference/api/pandas.DataFrame.itertuples.html
(defn itertuples
  "Iterate over DataFrame rows as namedtuples"
  [df & {:keys [index name*] :or {index true name* "Pandas"}}]
  (let [columns (ndarray/tolist (index/to-native-types (:columns df)))
        records (np/rec.fromarrays 
                 (mapv a2l (array/iter (:data df))) 
                 :names columns)]
    (array/array records)))

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
               (asarrays (map #((np/vectorize func :otypes [otype]) (a2n %))
                              (array/iter data)))))) 

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
                           (seq n) (np/delete (a2n $) n)
                           (seq m) ((np/vectorize
                                     #(np/delete (a2n %) m)) $))
                 :index (index-drop-fn (:index df) mvals m)
                 :columns (index-drop-fn (:columns df) nvals n)
                 :copy false))))

;; https://pandas.pydata.org/pandas-docs/stable/reference/api/pandas.DataFrame.equals.html
(defn equals
  "Test whether two objects contain the same elements"
  [df other]
  (and (= (:shape df) (:shape other))
       (reduce
        #(and %1 (let [a (array/item (:data df) %2)
                       other-a (array/item (:data other) %2)]
                   (and (= (:shape a) (:shape other-a))
                        (np/array-equal (a2n a) (a2n other-a)))))
        true
        (range (second (:shape df))))))

;; https://pandas.pydata.org/pandas-docs/stable/reference/api/pandas.DataFrame.take.html
(defn take*
  "Return the elements in the given positional indices along an axis"
  [df indices & {:keys [axis is-copy] :or {axis 0 is-copy true}}]
  (case axis
    0 (dataframe (map #(array/take* % indices) (array/iter (:data df)))
                 :columns (cond-> (:columns df) is-copy (index/copy))
                 :index (index/take* (:index df) indices)
                 :copy false)
    1 (dataframe (array/take* (:data df) indices)
                 :columns (index/take* (:columns df) indices)
                 :index (cond-> (:index df) is-copy (index/copy))
                 :copy false)))

;; https://pandas.pydata.org/pandas-docs/stable/reference/api/pandas.DataFrame.truncate.html
(defn truncate
  "Truncate a Series or DataFrame before and after some index value" 
  [df before after & {:keys [axis copy] :or {axis 0 copy true}}]
  (case axis
    (0 :index) (take* df (range (index/get-loc (:index df) before)
                                (index/get-loc (:index df) after))
                      :axis 0 :is-copy copy)
    (1 :columns) (take* df (range (index/get-loc (:columns df) before)
                                  (index/get-loc (:columns df) after))
                        :axis 1 :is-copy copy)))

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
  (if (= 0 axis)
    (let [_by (to-vector by)
          positions (map #(index/get-loc (:columns df) %) _by)
          columns (array/take* (:data df) positions)
          indices (if (= 1 (count columns))
                    (array/argsort (first columns))
                    (array/array (np/argsort (np/rec.fromarrays
                                              (map a2l (array/iter columns))
                                              :names _by))))
          index (index/index
                 (array/take* (index/array (:index df)) indices))
          data (map #(array/take* % (a2n indices)) (array/iter (:data df)))]
      (dataframe data
                 :columns (:columns df)
                 :index index
                 :copy false))))

;;; Combining / joining / merging
;;; Time series-related
;;; Plotting
;;; Serialization / IO / Conversion

;; https://pandas.pydata.org/pandas-docs/stable/reference/api/pandas.DataFrame.to_string.html
(defn to-string
  [df & {:keys [buf columns col-space header index
                na-rep formatters float-format sparsify index-names justify
                max-rows max-cols show-dimensions decimal line-width]
         :or {header true index true na-rep "NaN" index-names true
              show-dimensions false decimal \. col-space 2}}]
  (let [range-split-fn (fn [idx maximum]
                         (let [n (:size idx)]
                           (when (> n maximum)
                             (if (= 1 maximum)
                               {:range (range 1) :split 1}
                               (let [front (range (long (/ maximum 2)))
                                     back (range (- n (long (/ maximum 2))) n)]
                                 {:range (concat front back)
                                  :split (count front)})))))
        df-columns (if (some? columns)
                     (take* df (map #(index/get-loc (:columns df) %) columns)
                            :axis 1)
                     df)
        col-splits (when (some? max-cols)
                     (range-split-fn (:columns df-columns) max-cols))
        row-splits (when (some? max-rows)
                     (range-split-fn (:index df-columns) max-rows))
        df' (condas-> df-columns $
                      (some? col-splits) (take* $ (:range col-splits) :axis 1)
                      (some? row-splits) (take* $ (:range row-splits) :axis 0))
        df-strs (applymap df' str :otype :dtype/object)
        idx-strs (ndarray/tolist (index/to-native-types (:index df-strs)))
        idx-width (inc (max col-space 
                            (apply max (map (comp count str) idx-strs))))
        col-strs (as-> df-strs $
                   (index/to-native-types (:columns $))
                   (ndarray/tolist $)
                   (cons (spaces idx-width) $))
        col-widths (as-> df-strs $
                     (applymap $ #(max col-space (count %)) :otype :dtype/int64)
                     (map #(long (np/amax (a2n %))) (array/iter (:data $)))
                     (map-indexed #(max (count (nth col-strs (inc %1))) %2) $)
                     (cons idx-width $))
        records (itertuples df-strs)
        col-fn (fn [fmt align width s] 
                 (fmt (format (str "%" align width "s") s)))
        line-fn (fn [coll widths ffmt fmt falign align]
                  (let [cols (map-indexed 
                              (fn [i v]
                                (let [_fmt (if (zero? i) ffmt fmt)
                                      _align (if (zero? i) falign align)
                                      width (nth widths i)]
                                  (col-fn _fmt _align width v)))
                              coll)
                        line (string/join " " cols)]
                    (if (some? line-width)
                      (subs line 0 (min (ansi/visual-length line) line-width))
                      line)))
        split-line-fn (fn [split-idx coll widths ffmt fmt falign align]
                        (let [front (line-fn (take (max 2 (inc split-idx)) coll)
                                             (take (max 2 (inc split-idx)) widths)
                                             ffmt fmt falign align)
                              back (line-fn (drop (inc split-idx) coll)
                                            (drop (inc split-idx) widths)
                                            fmt fmt align align)
                              elipsis (line-fn ["  ...  "] 
                                               [7]
                                               ffmt fmt align align)]
                          (if (= 1 (count coll))
                            (str front elipsis)
                            (str front elipsis back))))
        col-split-idx (:split col-splits)
        row-split-idx (:split row-splits)
        headers (if (some? col-split-idx)
                  (split-line-fn col-split-idx col-strs col-widths
                                 ansi/bold-white ansi/bold-white
                                 "-" "")
                  (line-fn col-strs col-widths
                           ansi/bold-white ansi/bold-white
                           "-" ""))
        row-fn (fn [idx-label row-strs]
                 (let [coll (cons idx-label row-strs)]
                   (if (some? col-split-idx)
                     (split-line-fn col-split-idx coll col-widths
                                    ansi/bold-white identity "-" "")
                     (line-fn coll col-widths
                              ansi/bold-white identity "-" ""))))
        body (if (some? row-split-idx)
               (let [top (map-indexed
                          (fn [i rec] (row-fn (nth idx-strs i) (vals rec)))
                          (take row-split-idx (array/iter records)))
                     bottom (map-indexed
                             (fn [i rec] (row-fn (nth idx-strs
                                                      (+ i row-split-idx))
                                                 (vals rec)))
                             (drop row-split-idx (array/iter records)))
                     elipsis [(row-fn (dots (min 3 (dec idx-width)))
                                      (drop 1 (map #(dots (min 3 %)) 
                                                   col-widths)))]]
                 (if (zero? row-split-idx)
                   (concat bottom elipsis)
                   (concat top elipsis bottom)))
               (map-indexed 
                (fn [i rec] (row-fn (nth idx-strs i) (vals rec)))
                (array/iter records)))
        dimensions (when show-dimensions (format "[%d rows x %d columns]"
                                                 (first (:shape df))
                                                 (second (:shape df))))]    
    (cond-> (str headers \newline (string/join \newline body))
      (some? dimensions) (str \newline \newline dimensions))))

(defn show [df & args]
  (let [opts (apply array-map args)]
    (println (apply (partial to-string df) (mapcat seq opts)))))

(defn expr [df e]
  (cond 
    (scalar? e) (array/item (:data df) (index/get-loc (:columns df) e))
    (array/ndarray? e) (iloc df e)))
