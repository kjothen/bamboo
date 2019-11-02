(ns bamboo.series
  (:require [clojure.string :as string]
            [io.aviso.ansi :as ansi]
            [bamboo.array :as array]
            [bamboo.index :as index]
            [bamboo.objtype :refer [mask? ndarray? scalar? series? slice?]]
            [bamboo.utility :refer [dots front-back-split spaces]]
            [numcloj.core :as np]
            [numcloj.ndarray :as ndarray]))

;;;; https://pandas.pydata.org/pandas-docs/stable/reference/series.html

;; Forward Declarations
(declare copy)
(declare iat)
(declare iloc)

;;; Constructor

;; https://pandas.pydata.org/pandas-docs/stable/reference/api/pandas.Series.html 
(defn series
  "One-dimensional ndarray with axis labels (including time series)"
  [data & {:keys [index dtype name* copy fastpath]
           :or {copy false fastpath false}}]
  (if (series? data)
    (if copy (bamboo.series/copy data) data)
    (let [_array (array/array data :copy copy)]
      (merge
       {:objtype :objtype/series
        :array _array
        :index (if (some? index)
                 (index/index index :copy copy)
                 (index/rangeindex (:size _array)))
        :name* name*}
       (select-keys _array [:dtype :shape :size])))))

;;; Attributes 
(defn array [series] (:array series))

;;; Conversion

;; https://pandas.pydata.org/pandas-docs/stable/reference/api/pandas.Series.copy.html
(defn copy
  "Make a copy of this object’s indices and data"
  [series & {:keys [deep] :or {deep true}}]
  (bamboo.series/series (array/copy (:array series) :deep deep)
                        :index (index/copy (:index series) :deep deep)
                        :name* (:name* series)))

;; https://pandas.pydata.org/pandas-docs/stable/reference/api/pandas.Series.to_numpy.html
(defn to-numpy
  "An ndarray representing the values in this Series or Index"
  [series & {:keys [dtype copy] :or {copy false}}]
  (array/to-numpy (:array series) :dtype dtype :copy copy))

;; https://pandas.pydata.org/pandas-docs/stable/reference/api/pandas.Series.to_list.html
(defn to-list
  "Return a list of the values"
  [series & {:keys [dtype copy] :or {copy false}}]
  (array/iter (:array series)))

;;; Indexing, iteration

;; https://pandas.pydata.org/pandas-docs/stable/reference/api/pandas.Series.at.html           
(defn at
  "Access a single value for a row/column label pair"
  [series label]
  (iat series (index/get-loc (:index series) label)))

;; https://pandas.pydata.org/pandas-docs/stable/reference/api/pandas.Series.iat.html 
(defn iat
  "Access a single value for a row/column pair by integer position"
  [series i]
  (array/item (:array series) i))

;; https://pandas.pydata.org/pandas-docs/stable/reference/api/pandas.Series.loc.html
(defn loc
  "Access a group of rows and columns by label (s) or a boolean array. Allowed
   inputs are a single label, a list/array of labels, a slice obj of labels,
   a boolean array or a callable function"
  [series label]
  (let [label-type-fn (fn [label]
                        (cond
                          (scalar? label) :label
                          (mask? label) :mask
                          (ndarray? label) :labels
                          (fn? label) :callable
                          (slice? label) :slice
                          (sequential? label) :labels))
        i (case (label-type-fn label)
            :label (vector (index/get-loc (:index series) label))
            :labels (let [labels (array/array label)]
                      (map (partial index/get-loc (:index series))
                           (array/iter labels)))
            :slice (let [[start end]
                         (index/slice-locs (:index series)
                                           :start (:start label)
                                           :end (:end label))]
                     (range start (inc end)))
            :mask (ndarray/tolist (np/flatnonzero label))
            :callable (loc series (label series))
            (throw (ex-info (str "Must specify either a single label, "
                                 "a list of labels, an object slice, a boolean "
                                 "array or a callable function: " label)
                            {:type :ValueError})))]
    (iloc series i)))

;; https://pandas.pydata.org/pandas-docs/stable/reference/api/pandas.Series.iloc.html
(defn iloc
  "Purely integer-location based indexing for selection by position. Allowed
   inputs are an integer, a list/array of integers, a slice obj of integers,
   a boolean array or a callable function"
  [series i]
  (let [integer-type-fn (fn [i]
                          (cond
                            (int? i) :index
                            (mask? i) :mask
                            (ndarray? i) :indices
                            (fn? i) :callable
                            (slice? i) :slice
                            (sequential? i) :indices))]
    (case (integer-type-fn i)
      :index (iat series i)
      :indices (array/take* (:array series) i)
      :slice (array/take* (:array series)
                          (let [size (:size (:array series))
                                start (or (:start i) 0)
                                end (or (:end i) size)]
                            (range (max start 0) (inc (min end size)))))
      :mask (ndarray/tolist (np/flatnonzero i))
      :callable (iloc series (i series))
      (throw (ex-info (str "Must specify either a single integer, "
                           "a list/array of integers, an integer slice, "
                           "a boolean array or a callable function: " i )
                      {:type :ValueError})))))

;; https://pandas.pydata.org/pandas-docs/stable/reference/api/pandas.Series.items.html
(defn items
  "Lazily iterate over (index, value) tuples"
  [series]
  (partition 2 (interleave
                (ndarray/tolist (index/to-native-types (:index series)))
                (array/iter (:array series)))))

;; https://pandas.pydata.org/pandas-docs/stable/reference/api/pandas.Series.iteritems.html
(def iteritems items)

;; https://pandas.pydata.org/pandas-docs/stable/reference/api/pandas.Series.keys.html
(defn keys*
  "Return alias for index"
  [series]
  (:index series))

;; https://pandas.pydata.org/pandas-docs/stable/reference/api/pandas.Series.item.html
(defn item
  "Return the first element of the underlying data as a scalar"
  [series]
  (array/item (:array series) 0))

;;; Indexing, iteration

;; https://pandas.pydata.org/pandas-docs/stable/reference/api/pandas.Series.__iter__.html
(defn iter
  "Return an iterator of the values"
  [series]
  (array/iter (:array series)))

;;; Reindexing / selection / label manipulation

;; https://pandas.pydata.org/pandas-docs/stable/reference/api/pandas.Series.equals.html
(defn equals
  "Test whether two objects contain the same elements"
  [series other]
  (and (= (:shape series) (:shape other))
       (np/array-equal (to-numpy series) (to-numpy other))))

;; https://pandas.pydata.org/pandas-docs/stable/reference/api/pandas.Series.take.html
(defn take*
  "Return the elements in the given positional indices along an axis"
  [series indices & {:keys [axis is-copy]
                     :or {axis 0 is-copy true}}]
  (array/take* (:array series) indices))

;; https://pandas.pydata.org/pandas-docs/stable/reference/api/pandas.Series.to_string.html
(defn to-string
  [series & {:keys [buf na-rep float-format header index 
                    length dtype name* max-rows min-rows]
         :or {header true index true na-rep "NaN" length false}}]
  (let [row-splits (front-back-split (:size series)
                                     (if (some? max-rows)
                                       max-rows
                                       (:size series)))
        col-strs ((np/vectorize str) (to-numpy series))
        col-widths ((np/vectorize count) col-strs)
        max-col-width (long (np/amax col-widths))
        idx-strs (index/to-native-types (:index series))
        idx-widths ((np/vectorize count) idx-strs)
        max-idx-width (long (np/amax idx-widths))
        row-fn (fn [indices]
                 (map (fn [idx]
                        (let [idx-str (format (str "%" "-" max-idx-width "s")
                                              (ndarray/item idx-strs idx))
                              col-str (format (str "%"  max-col-width "s")
                                              (ndarray/item col-strs idx))]
                          (string/join "  " [(ansi/bold idx-str) 
                                             col-str])))
                      indices))
        body (if-some [split (:split row-splits)]
               (concat (row-fn (take split (:indices row-splits)))
                       [(string/join "  " 
                                    [(spaces max-idx-width)
                                     (format (str "%-" max-col-width "s") 
                                             (dots 3))])]
                       (row-fn (drop split (:indices row-splits))))
               (row-fn (:indices row-splits)))
        dimensions (when length
                     (format "Length: %d" (:size series)))]
    (cond-> (string/join \newline body)
      (some? dimensions) (str \newline \newline dimensions))))

(defn show [series & args]
  (let [opts (apply array-map args)]
    (println (apply (partial to-string series) (mapcat seq opts)))))
