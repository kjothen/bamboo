(ns bamboo.csv
  (:require [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [clojure.string :as string]
            [squeezer.core :as squeezer]
            [taoensso.tufte :as tufte]
            [bamboo.dataframe :as dataframe]
            [bamboo.utility :refer [condas-> in? nan? parse-long to-vector]]))

(defn parse-na
  "Returns true if `s` is in `na-values`,
   otheriwse returns false"
  [s na-values]
  (in? s na-values))

(defn parse-bool
  "Returns true if `s` is in `true-values`, 
   false if `s` is in `false-values`,
   otherwise returns nil"
  [s true-values false-values]
  (if (in? s true-values)
    true
    (when (in? s false-values)
      false)))

(defn parse-double
  "Returns the parsed double if `s` is a double and is not NaN,
   otherwise returns nil"
  [s na-values]
  (try (let [d (Double/parseDouble s)]
         (when-not (Double/isNaN d) d))
       (catch Exception e nil)))

(defn parse-string [s] (tufte/p :bamboo/csv.parse-string (when (seq s) s)))

(defn- read-string*
  "Returns a bool, long, double, non-empty string or nil"
  [s true-values false-values na-values skipinitialspace]
  (let [s' (if skipinitialspace (string/triml s) s)]
    (when-not (parse-na s' na-values)
      (if-some [b (parse-bool s' true-values false-values)]
        b
        (or (parse-long s') (parse-double s' na-values) (parse-string s'))))))

(defn- read-rows
  "Returns vectors of parsed rows, placing the headers (if any) in the first row"
  [reader sep quote-char opts]
  (let [{:keys [header usecols nrows true-values false-values na-values
                skip-blank-lines skipfooter skipinitialspace skiprows
                comment*]} opts
        col-fn (fn [col] (read-string* col
                                       true-values false-values na-values
                                       skipinitialspace))
        header-fn (fn [col] (read-string* col nil nil nil skipinitialspace))
        row-fn (fn [row] (pmap col-fn row))
        usecols-fn 
        (fn [rows]
          (map #(keep-indexed (fn [idx row] (when (in? idx usecols) row)) %)
               rows))
        skip-blank-lines-fn 
        (fn [rows] 
          (keep #(when-not (every? string/blank? %) %) rows))
        comment-fn
        (fn [rows] 
          (keep (fn [row]
                  (when (not= comment* (first (seq (first row))))
                    (take-while #(not= comment* (first (seq %))) row)))
                rows))
        rows (condas-> (doall (into [] (csv/read-csv reader
                                                     :separator sep
                                                     :quote quote-char))) $
                       (seq usecols) (usecols-fn $)
                       (pos? skiprows) (drop skiprows $)
                       (pos? skipfooter) (drop-last skipfooter $)
                       (true? skip-blank-lines) (skip-blank-lines-fn $)
                       (char? comment*) (comment-fn $)
                       (and (int? nrows) (pos? nrows)) (take (inc nrows) $))
        header-row (when header (map header-fn (nth rows header)))
        data-rows (pmap row-fn (if header (nthrest rows (inc header)) rows))]
    (if header
      (list* header-row data-rows) 
      data-rows)))

(defn- column-names
  [rows header? names prefix]
  (cond
    (some? names) (to-vector names)
    header? (first rows)
    :else (let [n (reduce max 0 (map count rows))]
            (map #(str prefix %) (range n)))))

(defn- pad-rows
  [rows columns-count]
  (map (fn [row]
         (let [row-column-count (count row)]
           (if (< row-column-count columns-count)
             (concat row (repeat (- columns-count row-column-count) nil))
             row)))
       rows))

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
   & {:keys [sep quote-char header names usecols prefix
             true-values false-values nrows
             na-values keep-default-na na-filter
             skiprows skipfooter
             skip-blank-lines skipinitialspace
             comment*]
      :or {sep \,  quote-char \" header 0 names nil prefix "col"
           true-values #{"True"} false-values #{"False"}
           keep-default-na true na-filter true
           skiprows 0 skipfooter 0
           skip-blank-lines true skipinitialspace false}}]
  ; TODO: delimiter, header (as list), index-col, usecols (strings, callables), 
  ;       squeeze, mangle-dupe-cols, dtype, engine, converters,
  ;       verbose, parse-dates, infer-datetime-format,
  ;       keep-date-col, date-parser, dayfirst, cache-dates, iterator,
  ;       chunksize, compression, thousands, decimal, lineterminator,
  ;       quoting, doublequote, escapechar, encoding, dialect,
  ;       error-bad-lines, warn-bad-lines, delim-whitespace, low-memory,
  ;       memory-map, float-precsion
  (with-open [reader (squeezer/reader-compr filepath-or-buffer)]
    (let [rows (read-rows
                reader sep quote-char
                {:header header
                 :usecols usecols
                 :nrows nrows
                 :true-values (set true-values)
                 :false-values (set false-values)
                 :na-values (na-value-filter
                             na-values keep-default-na na-filter)
                 :skiprows skiprows
                 :skipfooter skipfooter
                 :skip-blank-lines skip-blank-lines
                 :skipinitialspace skipinitialspace
                 :comment* comment*})
          header? (int? header)
          columns (column-names rows header? names prefix)
          data (if header? (rest rows) rows)]
      (dataframe/dataframe (pad-rows data (count columns)) :columns columns))))
