(ns bamboo.csv-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [io.aviso.ansi :as ansi]
            [bamboo.csv :as csv]
            [bamboo.array :as array]
            [bamboo.dataframe :as dataframe]
            [bamboo.index :as index]
            [numcloj.core :as np]))

(defn print-df [df] (dataframe/show df) (println))

(deftest header-tests
  (testing (str "Given CSV files with headers on zeroth row, nth row "
                "or no header row "
                "When I read the CSV file "
                "Then the same dataframe is produced")
    (let [fzero "bamboo/csv/header-row-0.csv"
          fone "bamboo/csv/header-row-1.csv"
          fnone "bamboo/csv/header-row-none.csv"
          df-zero (csv/read-csv (io/resource fzero))
          df-one (csv/read-csv (io/resource fone) :header 1)
          df-none (csv/read-csv (io/resource fnone)
                                :header nil :names ["a" "b" "c" "d"])
          df-prefix (csv/read-csv (io/resource fnone) :header nil :prefix "X")
          expected-prefix (mapv str (repeat "X")
                                (range (second (:shape df-prefix))))]
      (println (ansi/green (format "(read-csv \"%s\")" fzero)))
      (print-df df-zero)
      (println (ansi/green (format "(read-csv \"%s\" :header 1)" fone)))
      (print-df df-one)
      (is (dataframe/equals df-zero df-one))
      (println (ansi/green (format "(read-csv \"%s\" :header nil :names %s)"
                                   fnone ["a" "b" "c" "d"])))
      (print-df df-none)
      (is (dataframe/equals df-zero df-none))
      (println (ansi/green (format "(read-csv \"%s\" :header nil :prefix \"X\")"
                                   fnone)))
      (print-df df-prefix)
      (is (np/array-equal expected-prefix
                          (index/to-numpy (:columns df-prefix)))))
    ))

(deftest dtypes-tests
  (testing (str "Given a CSV file "
                "When I read the CSV file with and without options "
                "Then the dtypes are inferred correctly")
    (let [fdtypes "bamboo/csv/dtypes.csv"
          df (csv/read-csv (io/resource fdtypes))
          df-bool (csv/read-csv (io/resource fdtypes)
                                :true-values ["True" "Yes"] 
                                :false-values ["False" "No"])
          expected [:dtype/bool :dtype/int64
                    :dtype/float64 :dtype/object :dtype/object]
          expected-bool [:dtype/bool :dtype/int64
                         :dtype/float64 :dtype/object :dtype/bool]]
      (println (ansi/green (format "(read-csv \"%s\")" fdtypes)))
      (print-df df)
      (is (np/array-equal expected (array/to-numpy (:dtypes df))))

      (println (ansi/green (format (str "(read-csv \"%s\""
                                        " :true-values [\"True\" \"Yes\"]"
                                        " :false-values [\"False\" \"No\"])")
                                   fdtypes)))
      (print-df df-bool)
      (is (np/array-equal expected-bool (array/to-numpy (:dtypes df-bool)))))))

(deftest na-tests
  (testing (str "Given a CSV file with na values "
                "When I read the CSV file "
                "Then all nils are returned")
    (let [fna "bamboo/csv/na.csv"
          df (csv/read-csv (io/resource fna))
          df-nil (csv/read-csv (io/resource fna) :na-values "nil")
          df-obj (csv/read-csv (io/resource fna) :keep-default-na false)
          expected (vec (concat (repeat 16 :dtype/float64)
                                [:dtype/object]))
          expected-nil (vec (concat (repeat 17 :dtype/float64)))
          expected-obj (vec (concat (repeat 17 :dtype/object)))]
      (println (ansi/green (format "(read-csv \"%s\")" fna)))
      (print-df df)
      (is (np/array-equal expected (array/to-numpy (:dtypes df))))

      (println (ansi/green (format "(read-csv \"%s\" :na-values \"nil\")" fna)))
      (print-df df-nil)
      (is (np/array-equal expected-nil (array/to-numpy (:dtypes df-nil))))

      (println (ansi/green (format "(read-csv \"%s\" :keep-default-na false)"
                                   fna)))
      (print-df df-obj)
      (is (np/array-equal expected-obj (array/to-numpy (:dtypes df-obj)))))))

(deftest skip-limit-tests
  (testing (str "Given a CSV file "
                "When I read the CSV file with skip and nrow options "
                "Then the appropriate rows and data are returned"))
  (let [f "bamboo/csv/skip.csv"
        df (csv/read-csv (io/resource f) :header 1)
        df-comment (csv/read-csv (io/resource f) :comment* \#)
        df-skiprows (csv/read-csv (io/resource f) :skiprows 1)
        df-skipblanklines-false (csv/read-csv (io/resource f) 
                                              :header 1 
                                              :skip-blank-lines false)
        df-skipinitialspace-true (csv/read-csv (io/resource f)
                                               :header 1 
                                               :skipinitialspace true)
        df-skipfooter (csv/read-csv (io/resource f) :header 1 :skipfooter 1)]

    (println (ansi/green (format "(read-csv \"%s\" :header 1)" f)))
    (print-df df)
    (is (= [5 4] (:shape df)))

    (println (ansi/green (format "(read-csv \"%s\" :comment* \\#)" f)))
    (print-df df-comment)
    (is (dataframe/equals (dataframe/drop* df :columns ["c" "d"]) 
                          (dataframe/drop* df-comment :columns ["c" "d"])))

    (println (ansi/green (format "(read-csv \"%s\" :skiprows 1)" f)))
    (print-df df-skiprows)
    (is (dataframe/equals df df-skiprows))

    (println (ansi/green (format (str "(read-csv \"%s\" " 
                                      ":header 1 :skip-blank-lines false)") f)))
    (print-df df-skipblanklines-false)
    (is (= [6 4] (:shape df-skipblanklines-false)))
    (is (= " True" (dataframe/iat df 4 2)))

    (println (ansi/green (format (str "(read-csv \"%s\" "
                                      ":header 1 :skipinitialspace true)") f)))
    (print-df df-skipinitialspace-true)
    (is (true? (dataframe/iat df-skipinitialspace-true 4 2)))

    (println (ansi/green (format (str "(read-csv \"%s\" "
                                      ":header 1 :skipfooter 1)") f)))
    (print-df df-skipfooter)
    (is (= [4 4] (:shape df-skipfooter)))))

(deftest usecols-tests
  (let [f "bamboo/csv/usecols.csv"
        df (csv/read-csv (io/resource f))
        df-usecols (csv/read-csv (io/resource f) :usecols [0 1])]
    (println (ansi/green (format "(read-csv \"%s\")" f)))
    (print-df df)
    (is (= [3 4] (:shape df)))

    (println (ansi/green (format "(read-csv \"%s\" :usecols [0 1])" f)))
    (print-df df-usecols)
    (is (= [3 2] (:shape df-usecols)))
    
    ))
