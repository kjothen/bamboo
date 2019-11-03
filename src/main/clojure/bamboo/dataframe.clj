(ns bamboo.dataframe
  (:require [clojure.string :as string]
            [io.aviso.ansi :as ansi]
            [bamboo.array :as array]
            [bamboo.index :as index]
            [bamboo.objtype :refer [array? array-like? dataframe? mask? 
                                    ndarray? scalar? series? slice?]]
            [bamboo.series :as series]
            [bamboo.utility :refer [condas-> dots spaces to-vector]]
            [numcloj.core :as np]
            [numcloj.ndarray :as ndarray]))

;;;; https://pandas.pydata.org/pandas-docs/stable/reference/frame.html

;;; Forward Declarations
(declare take*)

;;; Constructor(s)
(defn- copy-data [df]
  (array/array
   (mapv series/copy (array/iter (:data df)))
   :dtype :dtype/object 
   :copy false))

(defn- construct-columns
  [data columns & {:keys [copy] :or {copy true}}]
  (cond
    (some? columns) (index/index columns :copy copy)
    (map? data) (index/index (keys data) :copy false)
    (array? data) (let [a0 (array/array (array/item data 0) :copy false)]
                    (index/rangeindex (:size a0)))
    (ndarray? data) (let [a0 (array/array (ndarray/item data 0) :copy false)]
                      (index/rangeindex (:size a0)))
    (sequential? data) (let [a0 (array/array (first data) :copy false)]
                         (index/rangeindex (:size a0)))
    :else (throw (ex-info "DataFrame constructor not properly called!"
                          {:type :ValueError}))))

(defn- construct-index 
  [data index & {:keys [copy] :or {copy true}}]
  (cond
    (some? index) (index/index index :copy copy)
    (map? data) 
    (let [index-fn #(cond
                      (series? %) (:index %)
                      (scalar? %) (index/rangeindex 1)
                      (array-like? %) (index/rangeindex (count %))
                      :else (throw (ex-info
                                    "DataFrame constructor not properly called!"
                                    {:type :ValueError})))]
      (reduce #(index/union* %1 (index-fn %2))
              (index-fn (second (first data)))
              (rest (vals data))))
    (array? data) (index/rangeindex (:size data))
    (ndarray? data) (index/rangeindex (:size data))
    (sequential? data) (index/rangeindex (count data))
    :else (throw (ex-info "DataFrame constructor not properly called!"
                          {:type :ValueError}))))

(defn- reindex-series
  [series index & {:keys [copy] :or {copy true}}]
  (if (index/equals (:index series) index)
    (series/series series :index index :copy copy)
    (let [a (np/full (:shape index) Double/NaN
                     :dtype (case (:dtype series)
                              :dtype/bool :dtype/object
                              :dtype/int64 :dtype/float64
                              (:dtype series)))
          indices (index/get-loc index (index/to-list (:index series)))]
      (np/copyto a (series/to-numpy series) :where indices)
      (series/series a :index index :copy false))))

(defn- reindex-array
  [a index & {:keys [copy] :or {copy true}}]
  (cond
    (= (:size a) (:size index)) (series/series (array/to-numpy a) 
                                               :index index 
                                               :copy copy)
    (> (:size a) (:size index)) (series/series
                                 (np/resize (array/to-numpy a) (:shape index))
                                 :index index
                                 :copy false)
    :else (let [_a (np/full (:shape index) Double/NaN
                            :dtype (case (:dtype a)
                                     :dtype/bool :dtype/object
                                     :dtype/int64 :dtype/float64
                                     (:dtype a)))]
            (np/copyto _a a :where (np/ones (:shape a) :dtype :dtype/bool))
            (series/series :data _a :index index :copy copy))))

(defn- construct-data
  [data index & {:keys [copy] :or {copy true}}]
  (let [series-fn (fn [data index copy]
                    (cond (series? data) (reindex-series data index :copy copy)
                          (array-like? data) (reindex-array
                                              (array/array data :copy false)
                                              index
                                              :copy copy)
                          :else (throw
                                 (ex-info
                                  "DataFrame constructor not properly called!"
                                  {:type :ValueError}))))
        array-fn (fn [data]
                   (cond (series? data) (series/to-list data)
                         (ndarray? data) (ndarray/tolist data)
                         (array? data) (array/iter data)
                         (sequential? data) data
                         :else (throw
                                (ex-info
                                 "DataFrame constructor not properly called!"
                                 {:type :ValueError}))))]
    (cond
      (map? data) (array/array (map #(series-fn % index copy) (vals data))
                               :dtype :dtype/object :copy false)
      (sequential? data) (array/array (map #(series-fn % index copy)
                                           (apply map vector 
                                                  (map array-fn data)))
                                      :dtype :dtype/object :copy false)
      :else (throw (ex-info "DataFrame constructor not properly called!"
                            {:type :ValueError})))))

;; https://pandas.pydata.org/pandas-docs/stable/reference/api/pandas.DataFrame.html#pandas.DataFrame
(defn dataframe
  "Two-dimensional size-mutable, potentially heterogeneous tabular data 
   structure with labeled axes (rows and columns). Arithmetic operations 
   align on both row and column labels. Can be thought of as a dict-like 
   container for Series objects. The primary pandas data structure."
  [data & {:keys [index columns dtype copy]
           :or {copy false}}]
  (if (dataframe? data)
    data
    (let [_columns (construct-columns data columns :copy copy)
          _index (construct-index data index :copy copy)
          _data (construct-data data _index :copy copy)]
      {:objtype :objtype/dataframe
       :data _data
       :columns _columns
       :index _index
       :shape [(:size _index) (:size _columns)]
       :dtypes (array/array (map :dtype (array/iter _data))
                            :dtype :dtype/object)})))

;;; Attributes and underlying data
;;; Conversion

;;; Indexing, iteration

;; https://pandas.pydata.org/pandas-docs/stable/reference/api/pandas.DataFrame.iat.html  
(defn iat
  "Access a single value for a row/column pair by integer position"
  [df index column]
  (series/iat (array/item (:data df) column) index))

;; https://pandas.pydata.org/pandas-docs/stable/reference/api/pandas.DataFrame.iloc.html
(defn iloc
  "Purely integer-location based indexing for selection by position"
  ([df index]
   (let [index-type (cond
                      (scalar? index) :index
                      (mask? index) :mask
                      (array-like? index) :indices
                      (fn? index) :callable
                      (slice? index) :slice)]
     (case index-type
       :index (series/series (map #(iat df index %) 
                                  (range (second (:shape df))))
                             :index (index/copy (:columns df))
                             :copy false)
       :indices (take* df index)
       :slice (take* df (range (:start index)
                               (+ (:end index) (:step index))
                               (:step index)))
       :mask (take* df (ndarray/tolist (np/flatnonzero index)))
       (throw (ex-info (str "Must specify either a single integer, "
                            "a list or array of integers, a slice of integers, "
                            "a boolean array or callable function: " index-type)
                       (:type :ValueError))))))
  ([df index column] (iat df index column)))

;; https://pandas.pydata.org/pandas-docs/stable/reference/api/pandas.DataFrame.at.html
(defn at
  "Access a single value for a row/column label pair"
  [df index column]
  (iat df 
       (index/get-loc (:index df) index) 
       (index/get-loc (:columns df) column)))

;; https://pandas.pydata.org/pandas-docs/stable/reference/api/pandas.DataFrame.loc.html
(defn loc
  "Access a group of rows and columns by label (s) or a boolean array"
  ([df index-label] (loc df index-label nil))
  ([df index-label column-label]
   (let [label-type-fn (fn [label index]
                         (cond
                           (scalar? label) :label
                           (mask? label) :mask
                           (array-like? label) :labels
                           (fn? label) :callable
                           (slice? label) :slice))
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
           nvals (when (some? n) (array/take* (index/array (:columns df)) n))
           index (if (some? mvals) (index/index mvals) (:index df))
           columns (if (some? nvals) (index/index nvals) (:columns df))]
       
       (if (= :label index-label-type)
         (let [data (condas-> (:data df) $
                              (some? n) (array/take* $ n)
                              (some? m) (array/array 
                                         (map #(series/iloc % (first m))
                                              (array/iter $))))]
           (series/series (array/iter data) 
                          :index columns))
         (let [data (condas-> (:data df) $
                              (some? n) (array/take* $ n)
                              (some? m) (array/array (map #(series/take* % m)
                                                          (array/iter $))))]
           (dataframe (apply array-map (interleave (index/to-list columns)
                                                   (array/iter data)))
                      :index index
                      :columns columns
                      :copy false)))))))

;; https://pandas.pydata.org/pandas-docs/stable/reference/api/pandas.DataFrame.itertuples.html
(defn itertuples
  "Iterate over DataFrame rows as namedtuples"
  [df & {:keys [index name*] :or {index true name* "Pandas"}}]
  (let [columns (ndarray/tolist (index/to-native-types (:columns df)))
        records (np/rec.fromarrays
                 (mapv series/to-list (array/iter (:data df)))
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
               (let [vfn (np/vectorize func :otypes [otype])
                     as (map #(vfn (series/to-numpy %)) (array/iter data))]
                 (array/array
                  (map #(series/series % :index (:index df)) as)
                  :dtype :dtype/object
                  :copy false)))))

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
                            (if (true? inplace) index (index/copy index))))
          index (index-drop-fn (:index df) mvals m)
          columns (index-drop-fn (:columns df) nvals n)
          series (condas-> (array/to-numpy (if (true? inplace)
                                             (:data df)
                                             (copy-data df))) $
                           (seq n) (np/delete $ n)
                           (seq m) ((np/vectorize
                                     #(np/delete (series/to-numpy %) m)) $))]

      (dataframe (apply array-map (interleave (index/to-list columns)
                                              (ndarray/tolist series)))
                    :index index
                    :columns columns
                    :copy false))))

;; https://pandas.pydata.org/pandas-docs/stable/reference/api/pandas.DataFrame.equals.html
(defn equals
  "Test whether two objects contain the same elements"
  [df other]
  (and (= (:shape df) (:shape other))
       (every? #(let [s (array/item (:data df) %)
                      other-s (array/item (:data other) %)]
                  (series/equals s other-s))
               (range (second (:shape df))))))

;; https://pandas.pydata.org/pandas-docs/stable/reference/api/pandas.DataFrame.take.html
(defn take*
  "Return the elements in the given positional indices along an axis"
  [df indices & {:keys [axis is-copy] :or {axis 0 is-copy true}}]
  (case axis
    (0 :index) (dataframe (apply array-map 
                                 (interleave (index/to-list (:columns df))
                                             (map #(series/take* % indices)
                                                  (array/iter (:data df)))))
                          :columns (cond-> (:columns df) is-copy (index/copy))
                          :index (index/take* (:index df) indices)
                          :copy false)
    (1 :columns) (let [columns (index/take* (:columns df) indices)]
                   (dataframe (apply array-map 
                                     (interleave (index/to-list columns)
                                                 (array/iter
                                                  (array/take* (:data df) 
                                                               indices))))
                              :columns columns
                              :index (cond-> (:index df) is-copy (index/copy))
                              :copy false))))

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
  (dataframe (map series/to-numpy (:data df))
             :columns (:index df)
             :index (:columns df)
             :copy copy))

;; https://pandas.pydata.org/pandas-docs/stable/reference/api/pandas.DataFrame.sort_values.html
(defn sort-values
  "Sort by the values along either axis"
  [df by & {:keys [axis ascending inplace kind na-position]
            :or {axis 0 ascending true inplace false
                 sorting :quicksort na-position :last}}]
  (let [_by (to-vector by)
        indices (case axis
                  (1 :columns) (map #(index/get-loc (:index df) %) _by)
                  (map #(index/get-loc (:columns df) %) _by))
        as (case axis
             (1 :columns) (array/array (map #(iloc df %) indices) 
                                       :dtype :dtype/object)
             (array/take* (:data df) indices))
        sorted-indices
        (if (= 1 (:size as))
          (array/argsort (series/array (array/item as 0)) :ascending ascending)
          (let [a (as-> (array/iter as) $
                    (map series/to-list $)
                    (np/argsort (np/rec.fromarrays $ :names _by)))]
            (array/array (if ascending a (np/flip a)) :copy false)))]
    (take* df sorted-indices :axis axis)))

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
                     (map #(long (np/amax (series/to-numpy %))) 
                          (array/iter (:data $)))
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

;;; Clojure Extensions

(defn show [df & args]
  (let [opts (apply array-map args)]
    (println (apply (partial to-string df) (mapcat seq opts)))))

(defn expr [df e]
  (cond
    (scalar? e) (array/item (:data df) (index/get-loc (:columns df) e))
    (ndarray? e) (iloc df e)))