(ns numcloj.ndarray
  (:require [clojure.string :as string]
            [numcloj.array.conversion :as conversion]
            [numcloj.array.item-manipulation :as item-manipulation]
            [numcloj.array-creation :as array-creation]
            [numcloj.functional :refer [vectorize]]
            [numcloj.api.statistics :refer [amax]]
            [utility.core :refer [front-back-split]]))

;;;; https://docs.scipy.org/doc/numpy/reference/arrays.ndarray.html

;;; Array methods

;; Array conversion
(def item conversion/item)
(def tolist conversion/tolist)
(def itemset conversion/itemset)
(def fill conversion/fill)
(def copy conversion/copy)

;; Shape manipulation

;; Item selection and manipulation
(def put item-manipulation/put)
(def take* item-manipulation/take*)
(def argsort item-manipulation/argsort)

;; Calculation

;;; Arithmetic, matrix multiplication, and comparison operations
;;; Special methods

(def ndarray? array-creation/ndarray?)

;;; Clojure extensions

(defn- array->string
  [a & {:keys [min-rows max-rows max-width]
        :or {min-rows 6 max-rows 1000 max-width 72}}]
  (let [row-splits (when (some? max-rows)
                     (front-back-split (:size a) max-rows :minimum min-rows))
        str-fn #(if (string? %) (str "'" % "'") ((fnil str "nil") %))
        strs ((vectorize str-fn :otypes [:dtype/object]) a)
        str-width (when-not (= :dtype/object (:dtype a))
                    (long (amax ((vectorize count) strs))))
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
                             sidx (item strs idx)
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
                (row-fn "array([" ", " (take split (:indices row-splits)))
                [(str " " (fmt-str-fn "..." "-" 1))]
                (row-fn ", " "])" (drop split (:indices row-splits))))
               (row-fn "array([" "])" (range (:size a))))]
    (string/join \newline body)))

(defn show [a & args]
  (let [opts (apply array-map args)]
    (println (apply (partial array->string a) (mapcat seq opts)))))
