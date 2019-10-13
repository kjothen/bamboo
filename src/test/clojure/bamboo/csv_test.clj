(ns bamboo.csv-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [bamboo.csv :as csv]
            [bamboo.array :as array]
            [bamboo.dataframe :as dataframe]
            [bamboo.index :as index]
            [bamboo.pprint :as pprint]
            [numcloj.core :as np]))

(deftest header-tests
  (testing (str "Given CSV files with headers on zeroth row, nth row "
                "or no header row "
                "When I read the CSV file "
                "Then the same dataframe is produced")
    (let [df-zero (csv/read-csv (io/resource "bamboo/csv/header-row-0.csv"))
          df-one (csv/read-csv (io/resource "bamboo/csv/header-row-1.csv")
                               :header 1)
          df-none (csv/read-csv (io/resource "bamboo/csv/header-row-none.csv")
                                :header nil
                                :names ["a" "b" "c" "d"])
          df-prefix (csv/read-csv (io/resource "bamboo/csv/header-row-none.csv")
                                  :header nil
                                  :prefix "X")
          expected-prefix (mapv str (repeat "X")
                                (range (second (:shape df-prefix))))]
      (pprint/pprint df-zero)
      (pprint/pprint df-one)
      (is (dataframe/equals df-zero df-one))
      (pprint/pprint df-none)
      (is (dataframe/equals df-zero df-none))
      (pprint/pprint df-prefix)
      (is (np/array-equal expected-prefix 
                          (index/to-numpy (:columns df-prefix)))))))

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
      (pprint/pprint df)
      (is (np/array-equal expected (array/to-numpy (:dtypes df))))
      (pprint/pprint df-bool)
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
                                [:dtype/object]))
          expected-nil (vec (concat (repeat 17 :dtype/float64)))
          expected-obj (vec (concat (repeat 17 :dtype/object)))]
      (pprint/pprint df)
      (is (np/array-equal expected (array/to-numpy (:dtypes df))))
      (pprint/pprint df-nil)
      (is (np/array-equal expected-nil (array/to-numpy (:dtypes df-nil))))
      (pprint/pprint df-obj)
      (is (np/array-equal expected-obj (array/to-numpy (:dtypes df-obj)))))))

(deftest skip-limit-tests
  (testing (str "Given a CSV file "
                "When I read the CSV file with skip and nrow options "
                "Then the appropriate rows and data are returned"))
  (let [f (io/resource "bamboo/csv/skip.csv")
        df (csv/read-csv f :header 1)
        df-comment (csv/read-csv f :comment* \#)
        df-skiprows (csv/read-csv f :skiprows 1)
        df-skipblanklines-false (csv/read-csv 
                                 f :header 1 :skip-blank-lines false)
        df-skipinitialspace-true (csv/read-csv 
                                  f :header 1 :skipinitialspace true)
        df-skipfooter (csv/read-csv f :header 1 :skipfooter 1)]

    (pprint/pprint df)
    (is (= [5 4] (:shape df)))
    (pprint/pprint df-comment)
    (is (dataframe/equals (dataframe/drop* df :columns ["c" "d"]) 
                          (dataframe/drop* df-comment :columns ["c" "d"])))
    (pprint/pprint df-skiprows)
    (is (dataframe/equals df df-skiprows))
    (pprint/pprint df-skipblanklines-false)
    (is (= [6 4] (:shape df-skipblanklines-false)))
    (is (= " True" (dataframe/iat df 4 2)))
    (pprint/pprint df-skipinitialspace-true)
    (is (true? (dataframe/iat df-skipinitialspace-true 4 2)))
    (pprint/pprint df-skipfooter)
    (is (= [4 4] (:shape df-skipfooter)))))

(deftest usecols-tests
  (let [df (csv/read-csv (io/resource "bamboo/csv/usecols.csv")
                         :usecols [0 1])]
    (pprint/pprint df)
    (is (= [3 2] (:shape df)))))
