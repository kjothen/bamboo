(ns bamboo.csv-test
  (:refer-clojure :exclude [drop])
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [bamboo.csv :as csv]
            [bamboo.array :as array]
            [bamboo.dataframe :as dataframe]
            [numcloj.core :as np]
            [numcloj.ndarray :as ndarray]))

(deftest header-tests
  (testing (str "Given CSV files with headers on zeroth row, nth row or no header row "
                "When I read the CSV file "
                "Then the same dataframe is produced")
    (let [df-zero (csv/read-csv (io/resource "bamboo/csv/header-row-0.csv"))
          df-one (csv/read-csv (io/resource "bamboo/csv/header-row-1.csv")
                               :header 1)
          df-none (csv/read-csv (io/resource "bamboo/csv/header-row-none.csv")
                                :header nil
                                :names ["a" "b" "c" "d"])]
      (is (dataframe/equals df-zero df-one))
      (is (dataframe/equals df-zero df-none)))))

(deftest dtypes-tests
  (testing (str "Given a CSV file "
                "When I read the CSV file with and without options "
                "Then the dtypes are inferred correctly")
    (let [df (csv/read-csv (io/resource "bamboo/csv/dtypes.csv"))
          df-bool (csv/read-csv (io/resource "bamboo/csv/dtypes.csv")
                                :true-values ["True" "Yes"] 
                                :false-values ["False" "No"])
          expected [:dtype/bool :dtype/int64
                    :dtype/float64 :dtype/object :dtype/object]
          expected-bool [:dtype/bool :dtype/int64
                         :dtype/float64 :dtype/object :dtype/bool]]
      (is (np/array-equal expected (array/to-numpy (:dtypes df))))
      (is (np/array-equal expected-bool (array/to-numpy (:dtypes df-bool)))))))

(deftest na-tests
  (testing (str "Given a CSV file with na values "
                "When I read the CSV file "
                "Then all nils are returned")
    (let [df (csv/read-csv (io/resource "bamboo/csv/na.csv"))
          df-nil (csv/read-csv (io/resource "bamboo/csv/na.csv")
                               :na-values "nil")
          df-obj (csv/read-csv (io/resource "bamboo/csv/na.csv")
                               :keep-default-na false)
          expected (vec (concat (repeat 16 :dtype/float64)
                                [:dtype/object :dtype/object]))
          expected-nil (vec (concat (repeat 18 :dtype/float64)))
          expected-obj (vec (concat (repeat 18 :dtype/object)))]
      (is (np/array-equal expected (array/to-numpy (:dtypes df))))
      (is (np/array-equal expected-nil (array/to-numpy (:dtypes df-nil))))
      (is (np/array-equal expected-obj (array/to-numpy (:dtypes df-obj)))))))