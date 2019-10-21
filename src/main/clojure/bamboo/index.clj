(ns bamboo.index
  (:require [bamboo.array :as array]
            [bamboo.objtype :as objtype]
            [bamboo.utility :refer [in? to-vector]]
            [numcloj.core :as np]
            [numcloj.ndarray :as ndarray])
  (:import (java.time DayOfWeek Duration Instant Period
                      LocalDate LocalDateTime LocalTime
                      ZonedDateTime ZoneId)
           (java.time.temporal TemporalAdjusters)
           (java.time.format DateTimeFormatter)))

;;;; https://pandas.pydata.org/pandas-docs/stable/reference/indexing.html

(declare copy)
(declare get-loc)
(declare to-numpy)

(defn- any-index? [a] (isa? objtype/bamboo-hierarchy (:objtype a) :objtype/index))
(defn- datetimeindex? [a] (= :objtype/datetimeindex (:objtype a)))

(defn- hash-array
  "Hash an arrray"
  [a]
  (zipmap (ndarray/tolist (array/to-numpy a :copy false)) 
          (range (first (:shape a)))))

;;; Index
(defn index
  "Immutable ndarray implementing an ordered, sliceable set. 
   The basic object storing axis labels for all pandas objects"
  [data & {:keys [dtype copy name* tupleize-cols]
           :or {copy false tupleizecols true}}]
  (if (any-index? data)
    (if (true? copy) (bamboo.index/copy data) data)
    (let [a (array/array data :dtype dtype :copy copy)]
      (merge {:objtype :objtype/index
              :data a
              :loc (hash-array a)
              :name* name*}
             (select-keys a [:shape :ndim
                             :size :nbytes])))))
  
;; Properties

; https://pandas.pydata.org/pandas-docs/stable/reference/api/pandas.Index.array.html
(defmulti array
  "The ExtensionArray of the data backing this Series or Index"
  :objtype)
(defmethod array :default [idx] (:data idx))
(defmethod array :objtype/rangeindex 
  [idx]
  (array/array (range (:start idx) (:stop idx) (:step idx))))

; https://pandas.pydata.org/pandas-docs/stable/reference/api/pandas.Index.dtype.html
(defmulti dtype
  "Return the dtype object of the underlying data"
  :objtype)
(defmethod dtype :default [idx] (:dtype idx))
(defmethod dtype :objtype/rangeindex [idx] :dtype/int64)

; https://pandas.pydata.org/pandas-docs/stable/reference/api/pandas.Index.T.html
(defn T
  "Return the transpose, which is by defintion self"
  [idx] idx)

; https://pandas.pydata.org/pandas-docs/stable/reference/api/pandas.Index.to_native_types.html
(defmulti to-native-types
  "Format specified values of self and return them"
  :objtype)

(defmethod to-native-types :default 
  [idx] 
  (array/array (map str (ndarray/tolist (to-numpy idx))) :dtype :dtype/object))

(defmethod to-native-types :objtype/datetimeindex
  [idx]
  (let [formats
        {:d (DateTimeFormatter/ofPattern "yyyy-MM-dd")
         :dt (DateTimeFormatter/ofPattern "yyyy-MM-dd'T'HH:mm:ss")
         :ms (DateTimeFormatter/ofPattern "yyyy-MM-dd'T'HH:mm:ss.SSSSSS")
         :ns (DateTimeFormatter/ofPattern "yyyy-MM-dd'T'HH:mm:ss.nnnnnnnnn")}
        nanos-in-seconds (long 1000000000)
        nanos-in-millis (long 1000000)
        to-datetime #(.atZone
                      (Instant/ofEpochSecond (quot % nanos-in-seconds)
                                             (rem % nanos-in-seconds))
                      (ZoneId/of "UTC"))
        format-ks (reduce #(let [zdt (to-datetime %2)
                                 hours? (pos? (.getHour zdt))
                                 minutes? (pos? (.getMinute zdt))
                                 seconds? (pos? (.getSecond zdt))
                                 millis? (pos? (quot (.getNano zdt)
                                                     nanos-in-millis))
                                 nanos? (pos? (rem (.getNano zdt)
                                                   nanos-in-millis))]
                             (cond
                               nanos? (conj %1 :ns)
                               millis? (conj %1 :ms)
                               (or hours? minutes? seconds?) (conj %1 :dt)
                               :else (conj %1 :d)))
                          #{}
                          (ndarray/tolist (to-numpy idx)))
        formatter (cond
                    (contains? format-ks :ns) (:ns formats)
                    (contains? format-ks :ms) (:ms formats)
                    (contains? format-ks :dt) (:dt formats)
                    :else (:d formats))]
    (array/array
     (map #(.format (to-datetime %) formatter) (ndarray/tolist (to-numpy idx)))
     :dtype :dtype/object)))

; https://pandas.pydata.org/pandas-docs/stable/reference/api/pandas.Index.to_numpy.html
(defn to-numpy
  "A NumCloj ndarray representing the values in this Series or Index"
  [idx & {:keys [copy] :or {copy false}}]
  (array/to-numpy (array idx)))

;; Modifying and computations

;https://pandas.pydata.org/pandas-docs/stable/reference/api/pandas.Index.copy.html
(defmulti copy
  "Make a copy of this object"
  :objtype)

(defmethod copy :default
  [idx & {:keys [name* deep dtype]
          :or {deep true}}]
  (if (some? (:data idx))
    (update-in idx [:data] array/array :copy true)
    idx))

; https://pandas.pydata.org/pandas-docs/stable/reference/api/pandas.Index.drop.html
(defmulti drop*
  "Make new Index with passed list of labels deleted"
  (fn [idx labels & {:keys [errors] :or {errors :raise}}] (:objtype idx)))

(defmethod drop* :default
  [idx labels & {:keys [errors] :or {errors :raise}}]
  (let [indices (keep #(get-loc idx % :errors errors) (to-vector labels))]
    (index (np/delete (to-numpy idx) indices))))

; https://pandas.pydata.org/pandas-docs/stable/reference/api/pandas.Index.equals.html
(defmulti equals
  "Determine if two Index objects contain the same elements"
  :objtype)

(defmethod equals :default
  [idx other]
  (np/array-equal (to-numpy idx) (to-numpy other)))

; https://pandas.pydata.org/pandas-docs/stable/reference/api/pandas.Index.take.html
(defmulti take*
  "Return a new Index of the values selected by the indices"
  (fn [idx indices & {:keys [axis allow-fill fill-value errors] 
                      :or {axis 0 allow-fill true errors :raise}}] 
    (:objtype idx)))

(defmethod take* :default
  [idx indices & {:keys [axis allow-fill fill-value errors]
                  :or {axis 0 allow-fill true errors :raise}}]
  (index (array/take* (array idx) indices)))

;; Compatibility with MultiIndex
;; Missing values
;; Conversion
(defn to-list [idx] ((comp ndarray/tolist array/to-numpy to-native-types) idx))

;; Sorting
;; Time-specific operations
;; Combining / joining / set operations
;; Selecting

; https://pandas.pydata.org/pandas-docs/stable/reference/api/pandas.Index.get_loc.html#pandas.Index.get_loc
(defmulti get-loc
  "Get integer location, slice or boolean mask for requested label"
  (fn [idx label & {:keys [errors method tolerance]}] (:objtype idx)))

(defmethod get-loc :default
  [idx label & {:keys [errors method tolerance]}]
  (let [i (get (:loc idx) label)]
    (when (and (nil? i) (= :raise errors))
      (throw (ex-info (str "[" label "] not found in axis")
                      {:type :KeyError})))
    i))

(defmethod get-loc :objtype/rangeindex
  [idx label & {:keys [method tolerance]}]
  ; TODO - deal with stepped/negative/non-zero starting indices..
  label)

;;; Numeric Index

;; RangeIndex
(defn rangeindex
  "Immutable Index implementing a monotonic integer range"
  [start & {:keys [stop step name*]
            :or {step 1}}]
  (let [idx {:objtype :objtype/rangeindex
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
  [data & {:keys [copy freq tz ambiguous name* dayfirst yearfirst]
           :or {copy false ambiguous :raise dayfirst false yearfirst false}}]
  (if (datetimeindex? data)
    (if (true? copy) 
      (datetimeindex (array/copy (array data))
                     :copy false 
                     :freq freq
                     :tz tz 
                     :ambiguous ambiguous 
                     :name* name*
                     :dayfirst dayfirst 
                     :yearfirst yearfirst)
      data)
    (let [a (array/array data :dtype :dtype/int64 :copy copy)]
      (merge {:objtype :objtype/datetimeindex
              :data a
              :loc (hash-array a)
              :name* name*
              :freq freq
              :tz tz}
             (select-keys a [:shape :ndim
                             :size :nbytes])))))
