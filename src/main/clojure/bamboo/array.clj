(ns bamboo.array
  (:require [clojure.string :as string]
            [numcloj.core :as np]
            [numcloj.ndarray :as ndarray]
            [bamboo.objtype :refer [array? mask? ndarray?]]
            [utility.core :refer [front-back-split]]))

;;;; An extension array for ndarrays

;;;; https://pandas.pydata.org/pandas-docs/version/0.24/reference/api/pandas.arrays.PandasArray.html
;;;; https://pandas.pydata.org/pandas-docs/version/0.23/generated/pandas.api.extensions.ExtensionArray.html

;;; Forward Declarations

(declare copy)
(declare from-numpy)
(declare to-numpy)

;;; Interface

(defn iter [a] (ndarray/tolist (to-numpy a)))
(defn item [a i] (ndarray/item (to-numpy a) i))

;;; Constructor

(defn- from-numpy [a]
  {:objtype :objtype/extension-array
   :dtype (:dtype a)
   :values a
   :size (:size a)
   :shape [(:size a) nil]})

;; https://pandas.pydata.org/pandas-docs/stable/reference/api/pandas.array.html
(defn array
  "Create an array"
  [values & {:keys [copy] :or {copy true}}]
  (cond
    (array? values) (if copy (bamboo.array/copy values) values)
    (ndarray? values) (let [a (if copy (np/copy values) values)]
                        (from-numpy a))
    :else (from-numpy (np/array values))))

;;; Methods

;; https://pandas.pydata.org/pandas-docs/version/0.24/reference/api/pandas.arrays.PandasArray.argsort.html
(defn argsort
  "Return the indices that would sort this array" 
  [a & {:keys [ascending kind]
        :or {ascending true}}]
  (let [_a (np/argsort (to-numpy a))]
    (array (if ascending _a (np/flip a)) :copy false)))

;; https://pandas.pydata.org/pandas-docs/version/0.24/reference/api/pandas.arrays.PandasArray.copy.html#pandas.arrays.PandasArray.copy
(defn copy
  "Return a copy of the array"
  [a & {:keys [deep] :or {deep true}}]
  (array (np/copy (to-numpy a)) :copy false))

;; https://pandas.pydata.org/pandas-docs/version/0.24/reference/api/pandas.arrays.PandasArray.take.html
(defn take*
  "Take elements from an array"
  [a indices & {:keys [allow-fill fill-value]
                :or {allow-fill false}}]
  (if (int? indices)
    (array [(item a indices)])
    (let [_indices (to-numpy (array indices :copy false))]
      (array (np/take* (to-numpy a) (ndarray/tolist _indices))))))
            
;; https://pandas.pydata.org/pandas-docs/version/0.24/reference/api/pandas.arrays.PandasArray.to_numpy.html
(defn to-numpy
  "Convert the PandasArray to a numpy.ndarray"
  [a & {:keys [dtype copy] :or {copy false}}]
  (:values (if copy (copy a) a)))

;;; Clojure Extensions

(defn- array->string
  [a & {:keys [min-rows max-rows max-width] 
        :or {min-rows 10 max-rows 100 max-width 72}}]  
  (let [row-splits (when (some? max-rows)
                     (front-back-split (:size a) max-rows :minimum min-rows))
        str-fn #(if (string? %) (str "'" % "'") ((fnil str "nil") %))
        strs ((np/vectorize str-fn :otypes [:dtype/object]) (to-numpy a))
        str-width (when-not (= :dtype/object (:dtype a))
                    (long (np/amax ((np/vectorize count) strs))))
        align (if (= :dtype/object (:dtype a)) "-" "")
        fmt-str-fn (fn [s align width] (format (str "%" align width "s") s))
        row-fn (fn [row-start row-end indices]
                 (let [len (count indices)]
                   (loop [i 0
                          line-width (count row-start)
                          line row-start
                          lines []]
                     (if (< i len)
                       (let [idx (nth indices i)
                             sidx (ndarray/item strs idx)
                             last? (= i (dec len))
                             s (cond-> sidx
                                 (some? str-width) (fmt-str-fn align str-width)
                                 (not last?) (str ", ")
                                 last? (str row-end))
                             width (count s)
                             next-width (+ line-width width)
                             newline? (> next-width max-width)]
                         (recur (unchecked-inc i)
                                (if newline? 1 next-width)
                                (if newline? " " (str line s))
                                (if newline? (conj lines (str line s)) lines)))
                       (if (< 1 line-width) (conj lines line) lines)))))
        body (if-some [split (:split row-splits)]
               (concat
                (row-fn "[" "," (take split (:indices row-splits)))
                [(str " " (fmt-str-fn "..." "-" 1))]
                (row-fn " " "]" (drop split (:indices row-splits))))
               (row-fn "[" "]" (range (:size a))))
        metadata (format "Length: %d, dtype: %s" (:size a) (name (:dtype a)))]
    (cond-> (string/join \newline (cons "<PandasArray>" body))
      (some? metadata) (str \newline metadata))))

(defn show [a & args]
  (let [opts (apply array-map args)]
    (println (apply (partial array->string a) (mapcat seq opts)))))
