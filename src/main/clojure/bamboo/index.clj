(ns bamboo.index
  (:require [bamboo.array :as array]
            [bamboo.dtype :as dtype]
            [bamboo.utility :refer [in? to-vector]]
            [lang.core :refer [ndarray-expr]]
            [numcloj.core :as np]
            [numcloj.ndarray :as nd]))

;;;; https://pandas.pydata.org/pandas-docs/stable/reference/indexing.html

(declare to-numpy)
(declare get-loc)

(defn- index? 
  "Return true if this is any type of index"
  [a]
  (and (map? a) (isa? dtype/bamboo-hierarchy (:dtype a) :dtype/index)))

(defn- copy-index
  "Copy an index (if it has data)"
  [idx]
  (if (some? (:data idx))
    (update-in idx [:data] array/array :copy true)
    idx))

(defn- hash-array
  "Hash an arrray"
  [a]
  (zipmap (nd/tolist (array/to-numpy a :copy false)) 
          (range (first (:shape a)))))

;;; Index
(defn index
  "Immutable ndarray implementing an ordered, sliceable set. 
   The basic object storing axis labels for all pandas objects"
  [data & {:keys [dtype copy name* tupleize-cols]
           :or {copy false tupleizecols true}}]
  (if (index? data)
    (if (true? copy) (copy-index data) data)
    (let [a (array/array data :dtype dtype :copy copy)]
      (merge {:dtype :dtype/index
              :data a
              :loc (hash-array a)
              :name* name*}
             (select-keys a [:shape :ndim
                             :size :nbytes])))))
  
;; Properties
(defn T
  "Return the transpose, which is by defintion self"
  [idx]
  idx)

(defmulti array
  "The ExtensionArray of the data backing this Series or Index"
  :dtype)
(defmethod array :default [idx] (:data idx))
(defmethod array :dtype/rangeindex [idx] 
  (array/array (range (:start idx) (:stop idx) (:step idx))))

(defn to-numpy
  "A NumCloj ndarray representing the values in this Series or Index"
  [idx & {:keys [copy] :or {copy false}}]
  (array/to-numpy (array idx)))

(defmulti dtype
  "Return the dtype object of the underlying data"
  :dtype)
(defmethod dtype :default [idx] (:dtype idx))
(defmethod dtype :dtype/rangeindex [idx] :dtype/int64)

;; Modifying and computations

; https://pandas.pydata.org/pandas-docs/stable/reference/api/pandas.Index.drop.html
(defmulti drop*
  "Make new Index with passed list of labels deleted"
  :dtype)
(defmethod drop* :default 
  [idx labels & {:keys [errors] :or {errors :raise}}] 
  (let [a (to-numpy idx)
        _labels (to-vector labels)
        indices (mapv #(get-loc idx %) _labels)]
    (if (and (= :raise errors) (not= (count _labels) (count indices)))
      (throw (ex-info "Not all labels could be found" 
                      {:type :KeyError :labels _labels}))
      (index (np/delete a indices)))))

; https://pandas.pydata.org/pandas-docs/stable/reference/api/pandas.Index.equals.html
(defmulti equals
  "Determine if two Index objects contain the same elements"
  :dtype)
(defmethod equals :default
  [idx other]
  (np/array-equal (to-numpy idx) (to-numpy other)))

;; Compatibility with MultiIndex
;; Missing values
;; Conversion
;; Sorting
;; Time-specific operations
;; Combining / joining / set operations
;; Selecting

; https://pandas.pydata.org/pandas-docs/stable/reference/api/pandas.Index.get_loc.html#pandas.Index.get_loc
(defmulti get-loc
  "Get integer location, slice or boolean mask for requested label"
  :dtype)
(defmethod get-loc :default 
  [idx label & {:keys [method tolerance]}]
  (get (:loc idx) label))
(defmethod get-loc :dtype/rangeindex
  [idx label & {:keys [method tolerance]}]
  ; TODO - deal with stepped/negative/non-zero starting indices..
  label)
;;; Numeric Index

;; RangeIndex
(defn rangeindex
  "Immutable Index implementing a monotonic integer range"
  [start & {:keys [stop step name*]
            :or {step 1}}]
  (let [idx {:dtype :dtype/rangeindex
             :name* name*
             :start (if (some? stop) start 0)
             :stop (if (some? stop) stop start)
             :step step}]
    (assoc idx :size (count (range (:start idx) (:stop idx) (:step idx))))))

;;; DatetimeIndex
(defn datetimeindex
  "Immutable ndarray of datetime64 data, represented internally as int64, 
   and which can be boxed to Timestamp objects that are subclasses of 
   datetime and carry metadata such as frequency information."
  [data & {:keys [copy freq tz ambiguous name dayfirst yearfirst]
           :or {copy false ambiguous :raise dayfirst false yearfirst false}}]
  (if (index? data)
    (if (true? copy) (copy-index data) data)
    (let [a (array/array data :dtype :dtype/int64 :copy copy)]
      (merge {:dtype :dtype/datetimeindex
              :data a
              :loc (hash-array a)
              :freq freq
              :tz tz}
             (select-keys a [:shape :ndim
                             :size :nbytes])))))


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

