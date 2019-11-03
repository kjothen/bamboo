(ns bamboo.index-test
  (:require [clojure.test :refer [deftest is]]
            [io.aviso.ansi :as ansi]
            [bamboo.core :as pd]
            [bamboo.index :refer [index rangeindex show
                                  to-native-types to-numpy]]
            [numcloj.core :as np]))

(def int64-index [6 7 8 9 10])
(def double64-index [6.1 6.2 6.3 6.4 6.5])
(def bool-index [true false true false true])
(def obj-index ["m" "n" "o" "p" "q"])

(defn print-index [idx] (show idx) (println))

(deftest index-test
  (is (np/array-equal int64-index (to-numpy (index int64-index))))
  (is (np/array-equal double64-index (to-numpy (index double64-index))))
  (is (np/array-equal bool-index (to-numpy (index bool-index))))
  (is (np/array-equal obj-index (to-numpy (index obj-index)))))

(deftest rangeindex-test
  (is (np/array-equal (range 10) (to-numpy (rangeindex 10))))
  (is (np/array-equal (range 10 20) (to-numpy (rangeindex 10 :stop 20))))
  (is (np/array-equal (range 10 20 5) 
                      (to-numpy (rangeindex 10 :stop 20 :step 5)))))

(defn- datetimeindex->array [idx] (to-native-types idx))

(defn- datetimeindex-spf-test
  [expected start periods freq]
  (let [actual (pd/date-range :start start :periods periods :freq freq)]
    (println (ansi/green (format (str "(date-range :start \"%s\" "
                                      ":periods %d :freq \"%s\")")
                                 start periods freq)))
    (print-index actual)
    (is (np/array-equal expected (datetimeindex->array actual)))))

(defn- datetimeindex-epf-test
  [expected end periods freq]
  (let [actual (pd/date-range :end end :periods periods :freq freq)]
    (println (ansi/green (format (str "(date-range :end \"%s\" "
                                      ":periods %d :freq \"%s\")")
                                 end periods freq)))
    (print-index actual)
    (is (np/array-equal expected (datetimeindex->array actual)))))

(defn- datetimeindex-sep-test
  [expected start end periods]
  (let [actual (pd/date-range :start start :end end :periods periods)]
    (println (ansi/green (format (str "(date-range :start \"%s\" "
                                      ":end \"%s \" :periods %d)")
                                 start end periods)))
    (print-index actual)
    (is (np/array-equal expected (datetimeindex->array actual)))))

(defn- datetimeindex-sef-test
  [expected start end freq]
  (let [actual (pd/date-range :start start :end end :freq freq)]
    (println (ansi/green (format (str "(date-range :start \"%s\" "
                                      ":end \"%s \" :freq \"%s\")")
                                 start end freq)))
    (print-index actual)
    (is (np/array-equal expected (datetimeindex->array actual)))))

;; start (s), end (e), periods (p) freq (f),
(deftest datetimeindex-test
  ; 1 year (Y)
  (let [start "2019-02-28"
        end "2021-12-31"
        expected-fwd ["2019-12-31" "2020-12-31" "2021-12-31"]
        expected-bwd ["2016-12-31" "2017-12-31" "2018-12-31"]
        expected-div ["2019-02-28T00:00:00"
                      "2020-07-30T12:00:00"
                      "2021-12-31T00:00:00"]]
    (datetimeindex-spf-test expected-fwd start 3 "Y")
    (datetimeindex-epf-test expected-bwd start 3 "Y")
    (datetimeindex-sef-test expected-fwd start end "Y")
    (datetimeindex-sef-test [] end start "Y")
    (datetimeindex-sep-test expected-div start end 3)
    (datetimeindex-sep-test (reverse expected-div) end start 3))

  ; 12 months (12M)
  (let [start "2019-02-28"
        end "2021-02-28"
        expected ["2019-02-28" "2020-02-29" "2021-02-28"]]
    (datetimeindex-spf-test expected start 3 "12M")
    (datetimeindex-sef-test expected start end "12M"))

  ; 1 month (M)
  (let [start "2019-02-28"
        end "2019-04-30"
        expected ["2019-02-28" "2019-03-31" "2019-04-30"]]
    (datetimeindex-spf-test expected start 3 "M")
    (datetimeindex-sef-test expected start end "M"))

  ; 4 weeks (4W)
  (let [start "2019-02-28"
        end "2019-04-28"
        expected ["2019-03-03" "2019-03-31" "2019-04-28"]]
    (datetimeindex-spf-test expected start 3 "4W")
    (datetimeindex-sef-test expected start end "4W"))

  ; 1 week (W)
  (let [start "2019-02-28"
        end "2019-03-17"
        expected ["2019-03-03" "2019-03-10" "2019-03-17"]]
    (datetimeindex-spf-test expected start 3 "W")
    (datetimeindex-sef-test expected start end "W"))

  ; 7 days (7D)
  (let [start "2019-02-28"
        end "2019-03-14"
        expected ["2019-02-28" "2019-03-07" "2019-03-14"]]
    (datetimeindex-spf-test expected start 3 "7D")
    (datetimeindex-sef-test expected start end "7D"))

  ; 1 day (D)
  (let [start "2019-02-28"
        end "2019-03-02"
        expected ["2019-02-28" "2019-03-01" "2019-03-02"]]
    (datetimeindex-spf-test expected start 3 "D")
    (datetimeindex-sef-test expected start end "D"))

  ; 24 hours (24H)
  (let [start "2019-02-28"
        end "2019-03-02"
        expected ["2019-02-28" "2019-03-01" "2019-03-02"]]
    (datetimeindex-spf-test expected start 3 "24H")
    (datetimeindex-sef-test expected start end "24H"))

  ; 1 hour (H)
  (let [start "2019-02-28"
        end "2019-02-28T02:00:00"
        expected ["2019-02-28T00:00:00"
                  "2019-02-28T01:00:00"
                  "2019-02-28T02:00:00"]]
    (datetimeindex-spf-test expected start 3 "H")
    (datetimeindex-sef-test expected start end "H"))

  ; 60 minutes (60T)
  (let [start "2019-02-28"
        end "2019-02-28T02:00:00"
        expected ["2019-02-28T00:00:00"
                  "2019-02-28T01:00:00"
                  "2019-02-28T02:00:00"]]
    (datetimeindex-spf-test expected start 3 "60T")
    (datetimeindex-sef-test expected start end "60T"))

  ; 1 minute (T)
  (let [start "2019-02-28"
        end "2019-02-28T00:02:00"
        expected ["2019-02-28T00:00:00"
                  "2019-02-28T00:01:00"
                  "2019-02-28T00:02:00"]]
    (datetimeindex-spf-test expected start 3 "T")
    (datetimeindex-sef-test expected start end "T"))
  
  ; 60 seconds (60S, 60min)
  (let [start "2019-02-28"
        end "2019-02-28T00:02:00"
        expected ["2019-02-28T00:00:00"
                  "2019-02-28T00:01:00"
                  "2019-02-28T00:02:00"]]
    (datetimeindex-spf-test expected start 3 "60S")
    (datetimeindex-sef-test expected start end "60S"))

  ; 1 second (S, mins)
  (let [start "2019-02-28"
        end "2019-02-28T00:00:02"
        expected ["2019-02-28T00:00:00"
                  "2019-02-28T00:00:01"
                  "2019-02-28T00:00:02"]]
    (datetimeindex-spf-test expected start 3 "S")
    (datetimeindex-sef-test expected start end "S"))

  ; 1000 millis (1000L, 1000ms)
  (let [start "2019-02-28"
        end "2019-02-28T00:00:02"
        expected ["2019-02-28T00:00:00"
                  "2019-02-28T00:00:01"
                  "2019-02-28T00:00:02"]]
    (datetimeindex-spf-test expected start 3 "1000L")
    (datetimeindex-sef-test expected start end "1000L"))

  ; 1 millis (L, ms)
  (let [start "2019-02-28"
        end "2019-02-28T00:00:00.002000"
        expected ["2019-02-28T00:00:00.000000"
                  "2019-02-28T00:00:00.001000"
                  "2019-02-28T00:00:00.002000"]]
    (datetimeindex-spf-test expected start 3 "L")
    (datetimeindex-sef-test expected start end "L"))

  ; 1,000,000 nanos (N)
  (let [start "2019-02-28"
        end "2019-02-28T00:00:00.002000"
        expected ["2019-02-28T00:00:00.000000"
                  "2019-02-28T00:00:00.001000"
                  "2019-02-28T00:00:00.002000"]]
    (datetimeindex-spf-test expected start 3 "1000000N")
    (datetimeindex-sef-test expected start end "1000000N"))
  
  ; 1 nano (N)
  (let [start "2019-02-28"
        end "2019-02-28T00:00:00.000000002"
        expected ["2019-02-28T00:00:00.000000000"
                  "2019-02-28T00:00:00.000000001"
                  "2019-02-28T00:00:00.000000002"]]
    (datetimeindex-spf-test expected start 3 "N")
    (datetimeindex-sef-test expected start end "N"))
  )
