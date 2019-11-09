(ns bamboo.series
  (:require [clojure.string :as string]
            [io.aviso.ansi :as ansi]
            [bamboo.array :as array]
            [bamboo.index :as index]
            [bamboo.objtype :refer [array-like? mask? scalar? series? slice?]]
            [bamboo.utility :refer [spaces]]
            [numcloj.core :as np]
            [numcloj.ndarray :as ndarray]
            [utility.core :refer [atom? front-back-split]]))

;;;; https://pandas.pydata.org/pandas-docs/stable/reference/series.html

;; Forward Declarations
(declare copy)
(declare iat)
(declare iat!)
(declare iloc)
(declare take*)
(declare to-numpy)

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
(defn values [series] (to-numpy series))

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

(defn at!
  "Set a single value for a row/column label pair,
   where `series` must be an atom"
  [series label]
  (iat! series (index/get-loc (:index @series) label)))

;; https://pandas.pydata.org/pandas-docs/stable/reference/api/pandas.Series.iat.html 
(defn iat
  "Access a single value for a row/column pair by integer position"
  [series i]
  (array/item (:array series) i))

(defn iat!
  "Set a single value for a row/column pair by integer position,
   where `series` must be an atom"
  [series i v]
  {:pre [(atom? series)]}
  (let [a (array/to-numpy (:array @series))]
    ;; TODO - check it can be cast first!
    (ndarray/itemset a i v)
    (swap! series
           assoc :array (array/array a :copy false))
    nil))

;; https://pandas.pydata.org/pandas-docs/stable/reference/api/pandas.Series.loc.html
(defn loc
  "Access a group of rows and columns by label (s) or a boolean array. Allowed
   inputs are a single label, a list/array of labels, a slice obj of labels,
   a boolean array or a callable function"
  [series label]
  (let [label-type-fn (fn [label] (cond
                                    (scalar? label) :label
                                    (mask? label) :mask
                                    (array-like? label) :labels
                                    (fn? label) :callable
                                    (slice? label) :slice))
        label-type (label-type-fn label)
        i (case label-type
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
    (if (= :label label-type)
      (iloc series i)
      (take* series i))))

;; https://pandas.pydata.org/pandas-docs/stable/reference/api/pandas.Series.iloc.html
(defn iloc
  "Purely integer-location based indexing for selection by position. Allowed
   inputs are an integer, a list/array of integers, a slice obj of integers,
   a boolean array or a callable function"
  [series i]
  (let [integer-type-fn (fn [i] (cond
                                  (int? i) :index
                                  (mask? i) :mask
                                  (array-like? i) :indices
                                  (fn? i) :callable
                                  (slice? i) :slice))]
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
  (bamboo.series/series
   (array/take* (:array series) indices)
   :index (index/take* (:index series) indices)))

;; https://pandas.pydata.org/pandas-docs/stable/reference/api/pandas.Series.to_string.html
(defn to-string
  [series & {:keys [buf na-rep float-format header index 
                    length dtype name* max-rows min-rows]
         :or {header true index true na-rep "NaN" length false}}]
  (let [row-splits (when (some? max-rows) 
                     (front-back-split (:size series) max-rows))
        col-strs ((np/vectorize str :otypes [:dtype/object]) (to-numpy series))
        col-width (long (np/amax ((np/vectorize count) col-strs)))
        idx-strs (index/to-native-types (:index series))
        idx-width (long (np/amax ((np/vectorize count) idx-strs)))
        spacer "  "
        fmt-str-fn (fn [align width s] (format (str "%" align width "s") s))        
        row-fn (fn [indices]
                 (map (fn [idx]
                        (let [idx-str (ndarray/item idx-strs idx)
                              col-str (ndarray/item col-strs idx)]
                          (string/join spacer
                                       [(ansi/bold 
                                         (fmt-str-fn "-" idx-width idx-str))
                                        (fmt-str-fn "" col-width col-str)])))
                        indices))
        body (if-some [split (:split row-splits)]
               (concat
                (row-fn (take split (:indices row-splits)))
                (let [elipsis (fmt-str-fn "-" col-width "...")]
                  (vector (string/join spacer [(spaces idx-width) elipsis]))
                (row-fn (drop split (:indices row-splits)))))
               (row-fn (range (:size series))))
        metadata (when length
                     (format "Length: %d" (:size series)))]
    (cond-> (string/join \newline body)
      (some? metadata) (str \newline metadata))))

;;; Clojure Extensions

(defn show [series & args]
  (let [opts (apply array-map args)
        s (apply (partial to-string series) (mapcat seq opts))]
    (println (str s
                  (if (:length opts) ", " \newline)
                  (format "dtype: %s" (name (:dtype series)))))))
