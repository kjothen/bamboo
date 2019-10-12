(ns bamboo.csv
  (:refer-clojure :exclude [read-string])
  (:require [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [clojure.string :as string]
            [taoensso.tufte :as tufte]
            [bamboo.dataframe :as dataframe]
            [bamboo.utility :refer [condas-> in? nan? parse-long to-vector]])
  (:refer-clojure :exclude [read-string]))

(defn parse-na
  "Returns true is `s` is in `na-values`,
   otheriwse returns false"
  [s na-values]
    (tufte/p
     :bamboo/csv.parse-na
     (in? s na-values)))

(defn parse-bool
  "Returns true if `s` is in `true-values`, 
   false if `s` is in `false-values`,
   otherwise returns nil"
  [s true-values false-values]
  (tufte/p
   :bamboo/csv.parse-bool
   (if (in? s true-values)
     true
     (if (in? s false-values)
       false))))

(defn parse-double 
  "Returns the parsed double if `s` is a double and is not NaN,
   otherwise returns nil"
  [s na-values]
  (tufte/p
   :bamboo/csv.parse-double
   (try (let [d (Double/parseDouble s)]
          (when-not (nan? d) d))
        (catch Exception e nil))))

(defn parse-string [s] (tufte/p :bamboo/csv.parse-string (when (seq s) s)))

(defn- read-string
  "Returns a bool, long, double, non-empty string or nil"
  [s true-values false-values na-values skipinitialspace]
  (tufte/p
   :bamboo/csv.read-string
   (let [s' (if skipinitialspace (string/triml s) s)]
     (when-not (parse-na s' na-values)
       (if-some [b (parse-bool s' true-values false-values)]
         b
         (or (parse-long s') (parse-double s' na-values) (parse-string s')))))))

(defn- read-rows
  [reader sep quote-char opts]
  ; note - profiling data is missing when using pmap 
  (let [{:keys [nrows true-values false-values na-values
                skipblanklines skipfooter skipinitialspace skiprows]} opts
        col-fn (fn [col] (read-string col
                                      true-values false-values na-values
                                      skipinitialspace))
        row-fn (fn [row] (pmap col-fn row))
        rows (condas->
              (doall (into [] (csv/read-csv reader
                                            :separator sep
                                            :quote quote-char))) $
              (pos? skipfooter) (drop-last skipfooter $)
              (pos? skiprows) (drop skiprows $)
              (true? skipblanklines) (remove nil? $)
              (and (int? nrows) (pos? nrows)) (take nrows $))]
    (pmap row-fn rows)))

(defn- col-names 
  [rows header names prefix]
  (cond
    (some? names) (to-vector names)
    (int? header) (nth rows header)
    :else (let [n (reduce max 0 (map count rows))]
            (map #(str prefix %) (range n)))))

(defn- row-data 
  [rows header names]
  (cond
    (some? names) (nthrest rows (if (int? header) (inc header) 0))
    (int? header) (nthrest rows (inc header))
    :else rows))

(defn- pad-row-data 
  [row-data max-col-count]
  (map (fn [row]
         (let [col-count (count row)]
           (if (< col-count max-col-count)
             (concat row (repeat (- max-col-count col-count) nil))
             row)))
       row-data))

(defn- na-value-filter
  [na-values keep-default-na na-filter]
  (when (true? na-filter)
    (let [default-na-values
          #{"#N/A" "#N/A N/A" "#NA" "-1.#IND" "-1.#QNAN" "-NaN" "-nan"
            "1.#IND" "1.#QNAN" "N/A" "NA" "NULL" "NaN" "n/a" "nan" "null"}]
      (if (true? keep-default-na)
        (if (some? na-values)
          (apply conj default-na-values (to-vector na-values))
          default-na-values)
        (when (some? na-values) (set (to-vector na-values)))))))

;; https://pandas.pydata.org/pandas-docs/stable/reference/api/pandas.read_csv.html
(defn read-csv
  "Construct a dataframe from a CSV file"
  [filepath-or-buffer
   & {:keys [sep quote-char header names prefix
             true-values false-values nrows
             na-values keep-default-na na-filter
             skiprows skipfooter
             skipblanklines skipinitialspace]
      :or {sep \,  quote-char \" names nil prefix "col"
           true-values #{"True"} false-values #{"False"}
           keep-default-na true na-filter true
           skiprows 0 skipfooter 0
           skipblanklines true skipinitialspace false}}]
  ; TODO: delimiter, header (as list), index-col, usecols, squeeze,
  ;       mangle-dupe-cols, dtype, engine, converters,
  ;       verbose, parse-dates, infer-datetime-format,
  ;       keep-date-col, date-parser, dayfirst, cache-dates, iterator,
  ;       chunksize, compression, thousands, decimal, lineterminator,
  ;       quoting, doublequote, escapechar, comment, encoding, dialect,
  ;       error-bad-lines, warn-bad-lines, delim-whitespace, low-memory,
  ;       memory-map, float-precsion
  (with-open [reader (io/reader filepath-or-buffer)]
    (let [rows (read-rows
                reader sep quote-char
                {:nrows nrows
                 :true-values (set true-values)
                 :false-values (set false-values)
                 :na-values (na-value-filter na-values keep-default-na na-filter)
                 :skiprows skiprows
                 :skipfooter skipfooter
                 :skipblanklines skipblanklines
                 :skipinitialspace skipinitialspace})
          header-row (if (or (some? names) (some? header)) header 0)
          columns (col-names rows header-row names prefix)
          row-data (row-data rows header-row names)]
      (dataframe/dataframe
       (apply map vector (pad-row-data row-data (count columns)))
       :columns columns))))
