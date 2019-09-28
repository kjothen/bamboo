(ns bamboo.index
  (:refer-clojure :rename {name clojure-name})
  (:require [bamboo.array :as array]))

;;;; https://pandas.pydata.org/pandas-docs/stable/reference/api/pandas.Index.html#pandas.Index

;;;
;;; Index objects
;;; TODO: Float64Index, Int64Index, UInt64Index, IntervalIndex, 
;;; MultiIndex, PeriodIndex, TimedeltaIndex

;;; Index
(defn index 
  "Immutable ndarray implementing an ordered, sliceable set. 
   The basic object storing axis labels for all pandas objects"
  [data & {:keys [dtype copy name tupleize-cols]
           :or {copy false tupleizecols true}}]
  (let [a (array/array data :dtype dtype :copy copy)]
    (merge {:dtype :dtype/index
            :data a
            :name name}
           (select-keys a [:shape :ndim
                           :size :nbytes]))))

;;; DatetimeIndex
(defn datetimeindex
  "Immutable ndarray of datetime64 data, represented internally as int64, 
   and which can be boxed to Timestamp objects that are subclasses of 
   datetime and carry metadata such as frequency information."
  [data & {:keys [copy freq tz ambiguous name dayfirst yearfirst]
           :or {copy false ambiguous :raise dayfirst false yearfirst false}}]
  {:dtype :dtype/datetimeindex
   :data data
   :freq freq
   :tz tz})

;;; RangeIndex
(defn rangeindex
  "Immutable Index implementing a monotonic integer range"
  [start & {:keys [stop step name]
            :or {step 1}}]
  {:dtype :dtype/rangeindex
   :name name
   :start (if (some? stop) start 0)
   :stop (if (some? stop) stop start)
   :step step})

;;; Attributes

(defn T [idx]
  "Return the transpose, which is by defintion self"
  idx)

(defmulti array
  "The ExtensionArray of the data backing this Series or Index"
  :dtype)
(defmethod array :default [idx] (:data idx))
(defmethod array :dtype/rangeindex [idx] 
  (array/array (range (:start idx) (:stop idx) (:step idx))))

(defmulti dtype
  "Return the dtype object of the underlying data"
  :dtype)
(defmethod dtype :default [idx] (:dtype idx))
(defmethod dtype :dtype/rangeindex [idx] :dtype/int64)

; (defmulti hasnans
;   "Return if I have any nans; enables various perf speedups"
;   :dtype)
; (defmethod dtype :default [idx] false)
; (defmethod dtype :dtype/float64index [idx] 
;   (array/any (array/isnan (:data idx))))

; (defn inferred-type
;   "Return a string of the type inferred from the values"
;   [idx]
;   (clojure-name (dtype index)))

; (defmulti is-monotonic-increasing
;   "Return if the index is monotonic increasing 
;   (only equal or increasing) values"
;   :dtype)
; (defmethod is-monotonic-increasing :default [idx] 
;   (apply <= (array idx)))
; (defmethod is-monotonic-increasing :dtype/rangeindex [idx] true)

; (defmulti is-monotonic-decreasing
;   "Return if the index is monotonic decreasing (only equal or decreasing) values"
;   :dtype)
; (defmethod is-monotonic-decreasing :default [idx] 
;   (apply >= (array idx)))
; (defmethod is-monotonic-decreasing :dtype/rangeindex [idx] false)

; (defn is-monotonic
;   "Alias for is_monotonic_increasing."
;   [idx] (is-monotonic-increasing idx))

; (defmulti is-unique
;   "Return if the index has unique values."
;   :dtype)
; (defmethod is-unique :default [idx] (apply distinct? (array idx)))
; (defmethod is-unique :dtype/rangeindex [idx] true)

; (defn nbytes
;   "Return the number of bytes in the underlying data"
;   [idx]
;   (-> idx (get :array) (get :nbytes)))

; (defn shape
;   "Return a tuple of the shape of the underlying data"
;   [idx]
;   (-> idx (get :array) (get :shape)))

; (defn ndim
;   "Number of dimensions of the underlying data, by definition 1"
;   [idx]
;   (-> idx (get :array) (get :ndim)))

; (defn size
;   "Return the number of elements in the underlying data"
;   [idx]
;   (-> idx (get :array) (get :size)))

; (defn empty [idx] (zero? (size idx)))

; (defmulti has-duplicates :index-type)
; (defmethod has-duplicates :default [idx] (not (is-unique idx)))
; (defmethod has-duplicates :index-type/range [idx] false)

; (defmulti is-all-dates :index-type)
; (defmethod is-all-dates :default [idx] false)
; (defmethod is-all-dates :index-type/datetime64-ns [idx] true)

; (defn name [idx] (:name idx))
; (defn names [idx] (list (name idx)))

; ;;; Post-Construction "Attributes"
; (defn T [idx] idx)
; (defn values [idx] (:array idx))

; ;;;
; ;;; Methods
; (defmethod all
;   "Return whether all elements are True"
;   :index-type)
; (defmulti all :default [idx] 
;   (throw (ex-info (str "cannot perform all with this index type: " 
;                        (:index-type idx)) {:type :TypeError})))
; (defmulti all :index-type/bool [idx] (array/all (:array idx)))
; (defmulti all :index-type/datetime64ns [idx] true)
; (defmulti all :index-type/int64 [idx] (array/all (:array idx)))
; (defmulti all :index-type/object [idx] (some? (array/all (:array idx))))
; (defmulti all :index-type/range [idx] true)

; (defmethod any
;   "Return whether any element is True"
;   :index-type)
; (defmulti all :default [idx]
;   (throw (ex-info (str "cannot perform any with this index type: "
;                        (:index-type idx)) {:type :TypeError})))
; (defmulti any :index-type/bool [idx] (array/any (:array idx)))
; (defmulti any :index-type/datetime64ns [idx] true)
; (defmulti any :index-type/int64 [idx] (array/any (:array idx)))
; (defmulti any :index-type/object [idx] (some? (array/any (:array idx))))
; (defmulti any :index-type/range [idx] true)

; (defmethod argmax
;   "Return ~~an ndarray of~~ the maximum argument indexer"
;   [idx]
;   (array/argmax (:array idx)))

; (defmethod argmin
;   "Return ~~an ndarray of~~ the minimum argument indexer"
;   [idx]
;   (array/argmin (:array idx)))

; (defmethod argsort
;   "Return the integer indices that would sort the index"
;   [idx]
;   (array/argsort (:array idx)))

; ;; Indices
; (defn from-sequential [vs & {:keys [dtype copy name tupleize-cols]}])

