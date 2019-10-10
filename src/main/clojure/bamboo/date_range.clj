(ns bamboo.date-range
  (:require [clojure.string :as string]
            [bamboo.array :as array]
            [bamboo.index :as index]
            [bamboo.utility :refer [parse-long]])
  (:import (java.time DayOfWeek Duration Instant Period
                      LocalDate LocalDateTime LocalTime
                      ZonedDateTime ZoneId)
           (java.time.temporal TemporalAdjusters)))

(def default-freq "D")
(def default-tz "UTC")

;; Constants
(def ^:const nanos-in-second (long 1000000000))

;; Helpers
(defn- as-datetime64ns 
  "Convert a ZonedDateTime to a datetime64ns"
  [zdt]
  (let [instant (.toInstant zdt)]
    (+ (* nanos-in-second (.getEpochSecond instant)) (.getNano instant))))

(defn- parse-datetime
  "Parse a datetime string into a ZonedDateTime"
  [dt tz]
  (try (ZonedDateTime/of (LocalDateTime/parse dt)
                         (ZoneId/of (or tz default-tz)))
       (catch Exception _
         (ZonedDateTime/parse dt))))

(defn- parse-date
  "Parse a date string into a ZonedDateTime"
  [dt tz]
  (try (ZonedDateTime/of (LocalDate/parse dt) 
                         (LocalTime/MIN) 
                         (ZoneId/of (or tz default-tz)))
       (catch Exception _ 
         ; fallback to parsing a datetime
         (parse-datetime dt tz))))

(defn- step [start freq]
  (if-let [[_ amount unit] (re-find #"^(-?\d*)(\w+)" freq)]
    (let [n (if (string/blank? amount) 1 (parse-long amount))]
      (case unit
        "Y" [(.with start (TemporalAdjusters/lastDayOfYear)) 
             (java.time.Period/ofYears n)]
        "M" [(.with start (TemporalAdjusters/lastDayOfMonth))
             (java.time.Period/ofMonths n)]
        "W" [(.with start (TemporalAdjusters/next (DayOfWeek/SUNDAY)))
             (java.time.Period/ofWeeks n)]
        "D" [start (java.time.Period/ofDays n)]
        "H" [start (java.time.Duration/ofHours n)]
        ("T","min") [start (java.time.Duration/ofMinutes n)]
        "S" [start (java.time.Duration/ofSeconds n)]
        ("L","ms") [start (java.time.Duration/ofMillis n)]
        ("U","us") [start (java.time.Duration/ofNanos (* 1000 n))]
        "N" [start (java.time.Duration/ofNanos n)]
        [start (java.time.Period/ofDays n)]))
    (throw (ex-info "Invalid freq: " {:freq freq}))))

(defn- forward
  [start freq periods]
  (let [[adj-start step] (step start (or freq default-freq))]
    (reduce (fn [m _] (conj m (.plus (last m) step)))
            [adj-start] (range (dec periods)))))

(defn- backward
  [end freq periods]
  (let [[adj-end step] (step end (or freq default-freq))]
    (reverse
     (reduce (fn [m _] (conj m (.minus (last m) step)))
             [adj-end] (range (dec periods))))))

(defn- bounded
  [start end freq]
  (let [[adj-start step] (step start (or freq default-freq))]
    (if (.isBefore start end)
      (loop [i 1
             result [adj-start]]
        (let [next (.plus adj-start (.multipliedBy step i))]
          (if (or (.isBefore next end) (.isEqual next end))
            (recur (inc i) (conj result next))
            result)))
      (loop [i 1
             result [adj-start]]
        (let [next (.plus adj-start (.multipliedBy step i))]
          (if (or (.isAfter next end) (.isEqual next end))
            (recur (inc i) (conj result next))
            result))))))

(defn- stepped
  [start end periods]
  (let [before? (.isBefore start end)
        duration (if before? 
                   (Duration/between start end) 
                   (Duration/between end start))
        step (.dividedBy duration (dec periods))]
    (if before?
      (reduce (fn [m _] (conj m (.plus (last m) step)))
              [start] (range (dec periods)))
      (reduce (fn [m _] (conj m (.minus (last m) step)))
               [start] (range (dec periods))))))

; Of the four parameters start (s), end (e), 
; periods (p) and freq (f), exactly three must
; be specified. Note: freq (f) default to "D""
; if not specified.
;
; #  | s | e | f | p | ? |
; ---+---+---+---+---+---+-------------------------
; 15 | x | x | x | x | N |
; 14 | x | x | x |   | Y | #{:start :end :freq}
; 13 | x | x | D | x | Y | #{:start :end :periods}
; 12 | x | x | D |   | Y | #{:start :end}
; 11 | x |   | x | x | Y | #{:start :freq :periods}
; 10 | x |   | x |   | N |
;  9 | x |   | d | x | Y | #{:start :periods}
;  8 | x |   |   |   | N |
;  7 |   | x | x | x | Y | #{:end :freq :periods}
;  6 |   | x | x |   | N |
;  5 |   | x | d | x | Y | #{:end :periods}
;  4 |   | x |   |   | N |
;  3 |   |   | x | x | N |
;  2 |   |   | x |   | N |
;  1 |   |   |   | x | N |
;  0 |   |   |   |   | N |

(defn date-range
  "Return a fixed frequency datetimeindex"
  [& {:keys [start end periods freq tz normalize name closed] :as all}]
  (let [start' (when (some? start) (parse-date start tz))
        end' (when (some? end) (parse-date end tz))
        method-keys (select-keys all [:start :end :freq :periods])
        some-keys (into {} (filter (comp some? second) method-keys))
        data (condp = (set (keys some-keys))
               #{:start :periods}        (forward start' freq periods)
               #{:start :freq :periods}  (forward start' freq periods)
               #{:start :end}            (bounded start' end' freq)
               #{:start :end :periods}   (stepped start' end' periods)
               #{:start :end :freq}      (bounded start' end' freq)
               #{:end :periods}          (backward end' freq periods)
               #{:end :freq :periods}    (backward end' freq periods)
               (throw (ex-info
                       (str "Of the four parameters start, end, periods,"
                            " and freq, exactly three must be specified")
                       {:type :ValueError})))]
    ; return a datetimeindex
    (index/datetimeindex 
     (array/array (map as-datetime64ns data) :dtype :dtype/int64) 
     tz)))
