(ns bamboo.index
  (:require [clojure.set :refer [union]]
            [bamboo.array :as array]
            [bamboo.objtype :as objtype :refer [any-index? datetimeindex? rangeindex?]]
            [bamboo.utility :refer [condas-> in? spaces to-vector]]
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
(declare to-list)
(declare to-native-types)
(declare to-numpy)
(declare rangeindex)

(defn- hash-values
  "Hash an arrray"
  [a]
  ; TODO - non-unique indices
  (zipmap (array/iter a) (range (first (:shape a)))))

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
              :loc (hash-values a)
              :name* name*}
             (select-keys a [:shape :size])))))
  
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
  ; ((np/vectorize str :otypes [:dtype/object]) 
  ;  (to-numpy idx))
  (to-numpy idx))

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
    ((np/vectorize #(.format (to-datetime %) formatter) :otypes [:dtype/object])
     (to-numpy idx))))

 ; https://pandas.pydata.org/pandas-docs/stable/reference/api/pandas.Index.to_numpy.html
(defn to-numpy
  "A NumCloj ndarray representing the values in this Series or Index"
  [idx & {:keys [copy] :or {copy false}}]
  (array/to-numpy (array idx)))

;; Modifying and computations

;https://pandas.pydata.org/pandas-docs/stable/reference/api/pandas.Index.copy.html
(defmulti copy
  "Make a copy of this object"
  (fn [idx & {:keys [name* deep dtype] :or {deep true}}] (:objtype idx)))

(defmethod copy :default
  [idx & {:keys [name* deep dtype] :or {deep true}}]
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
(defn to-list 
  "Return a list of the values"
  [idx] 
  ((comp ndarray/tolist to-numpy) idx))

(defn map*
  "Map values using input correspondence (a dict, Series, or function)"
  [idx mapper & {:keys [na-action] :or {na-action :ignore}}]
  (index ((np/vectorize mapper) (to-list idx))))

;; Sorting
;; Time-specific operations
;; Combining / joining / set operations

; https://pandas.pydata.org/pandas-docs/stable/reference/api/pandas.Index.union.html
(defmulti union*
  "Form the union of two Index objects"
  (fn [idx other & {:keys [sort*]}] (:objtype idx)))

(defmethod union* :default
  [idx other & {:keys [sort*]}]
  (index (vec (apply sorted-set (union (to-list idx) (to-list other))))))

(defmethod union* :objtype/rangeindex
  [idx other & {:keys [sort*]}]
  (if (and (rangeindex? other)
           (= (:step idx) (:step other))
           (or (<= (:start idx) (:stop other))
               (<= (:start other) (:stop idx))))
    (rangeindex (min (:start idx) (:start other))
                :stop (max (:stop idx) (:stop other))
                :step (:step idx))
    (index (vec (apply sorted-set (union (to-list idx) (to-list other)))))))

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
  (+ (:start idx) (* label (:step idx)))
  label)

;; https://pandas.pydata.org/pandas-docs/stable/reference/api/pandas.Index.slice_locs.html
(defmulti slice-locs
  "Compute slice locations for input labels"
  (fn [idx & {:keys [start end step kind]}] (:objtype idx)))

(defmethod slice-locs :default
  [idx & {:keys [start end step kind]}]
  [(if (nil? start) 0 (get-loc idx start))
   (if (nil? end) (dec (:size idx)) (get-loc idx end))])


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
              :loc (hash-values a)
              :name* name*
              :freq freq
              :tz tz}
             (select-keys a [:shape :size])))))

;;; Clojure Extensions

(defn- index->string
  [idx objtype-str objfields-str max-rows max-width]
  (let [start-obj (str objtype-str "([")
        end-obj-tokens (if (some? objfields-str) 
                         ["], " objfields-str]
                         ["]"]) 
        elipsis "..."
        sep ", "
        data (condas-> (ndarray/tolist (to-native-types idx)) $
                       (> (count $) max-rows) (concat (take 10 $)
                                                      [elipsis]
                                                      (take-last 10 $)))
        len (count data)]
    (loop [i 0
           data-tokens [start-obj]]
      (if-not (< i len)
        (let [lns (reduce (fn [[s ln-width] token]
                            (let [end? (in? token end-obj-tokens)
                                  token-width (count token)
                                  width (+ ln-width token-width)
                                  indent (spaces (if end?
                                                   (dec (count start-obj))
                                                   (count start-obj)))]
                              (if (> width max-width)
                                [(str s \newline indent token)
                                 (+ (count indent) token-width)]
                                [(str s token)
                                 width])))
                          ["" 0]
                          (concat data-tokens end-obj-tokens))]
          (apply str (first lns)))
        (let [datum (nth data i)
              elipsis? (= elipsis datum)
              s (if elipsis?
                  (format (str "%-" max-width "s") elipsis)
                  (if (string? datum)
                    (format "'%s'" datum)
                    (format "%s" datum)))
              delimiter (if elipsis?
                          ""
                          (if (< i (dec len)) sep ""))]
          (recur (inc i)
                 (conj data-tokens (str s delimiter))))))))

(defmulti show (fn [idx & args] (:objtype idx)))

(defmethod show :default [idx & args] 
  (let [{:keys [max-rows max-width]
         :or {max-rows 100 max-width 80}} (apply array-map args)]
    (println (index->string idx
                            "Index"
                            (format "dtype='%s')"
                                    (name (get-in idx [:data :dtype])))
                            max-rows
                            max-width))))

(defmethod show :objtype/rangeindex [idx & args]
  (println (format "RangeIndex(start=%d, stop=%d, step=%d)"
                   (:start idx) (:stop idx) (:step idx))))

(defmethod show :objtype/datetimeindex [idx & args]
  (let [{:keys [max-rows max-width]
         :or {max-rows 100 max-width 80}} (apply array-map args)]
    (println (index->string idx 
                            "DatetimeIndex"
                            (format "dtype='%s', freq='%s')"
                                    (name (get-in idx [:data :dtype]))
                                    (:freq idx))
                            max-rows 
                            max-width))))
