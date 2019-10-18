(ns bamboo.date-range
  (:require [clojure.string :as string]
            [bamboo.array :as array]
            [bamboo.index :as index]
            [bamboo.utility :refer [parse-long]])
  (:import (java.time DayOfWeek Duration Instant Period
                      LocalDate LocalDateTime LocalTime
                      ZonedDateTime ZoneId)
           (java.time.temporal TemporalAdjusters)
           (java.util.function UnaryOperator)))

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

(def ^:private TemporalAdjusterNone (TemporalAdjusters/ofDateAdjuster
                                     (reify UnaryOperator
                                       (apply [this arg] arg))))

(defn- step [freq]
  (if-let [[_ amount unit] (re-find #"^(-?\d*)(\w+)" freq)]
    (let [n (if (string/blank? amount) 1 (parse-long amount))]
      (case unit
        "Y" [(java.time.Period/ofYears n) 
             (TemporalAdjusters/lastDayOfYear)]
        "M" [(java.time.Period/ofMonths n) 
             (TemporalAdjusters/lastDayOfMonth)]
        "W" [(java.time.Period/ofWeeks n) 
             (TemporalAdjusters/nextOrSame (DayOfWeek/SUNDAY))]
        "D" [(java.time.Period/ofDays n) TemporalAdjusterNone]
        "H" [(java.time.Duration/ofHours n) TemporalAdjusterNone]
        ("T","min") [(java.time.Duration/ofMinutes n) TemporalAdjusterNone]
        "S" [(java.time.Duration/ofSeconds n) TemporalAdjusterNone]
        ("L","ms") [(java.time.Duration/ofMillis n) TemporalAdjusterNone]
        ("U","us") [(java.time.Duration/ofNanos (* 1000 n)) 
                    TemporalAdjusterNone]
        "N" [(java.time.Duration/ofNanos n) TemporalAdjusterNone]
        [(java.time.Period/ofDays n) TemporalAdjusterNone]))
    (throw (ex-info "Invalid freq: " {:freq freq}))))

(defn- forward
  [start freq periods]
  (let [[step adj] (step (or freq default-freq))]
    (reduce (fn [m _] (conj m (.with (.plus (last m) step) adj)))
            [(.with start adj)] (range (dec periods)))))

(defn- backward
  [end freq periods]
  (let [[step adj] (step (or freq default-freq))]
    (reverse
     (reduce (fn [m _] (conj m (.with (.minus (last m) step) adj)))
             [(.with (.minus end step) adj)] (range (dec periods))))))

(defn- bounded
  [start end freq]
  (let [[step adj] (step (or freq default-freq))]
    (when (.isBefore (.with start adj) end)
      (loop [i 1
             result [(.with start adj)]]
        (let [next (.with (.plus start (.multipliedBy step i)) adj)]
          (if (or (.isBefore next end) (.isEqual next end))
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
     :freq freq 
     :tz tz)))
