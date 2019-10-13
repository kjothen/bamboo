(ns bamboo.dataframe-test
  (:require [clojure.test :refer [deftest is]]
            [bamboo.dataframe :refer [dataframe drop* equals]]
            [bamboo.index :as index]
            [numcloj.ndarray :as ndarray])
  (:import (java.util Arrays)))

(def vs [[false true false true false]
         [99.0 -42.0 0.0 Double/NaN 16.66]
         [0 -1 42 99 23]
         [23 99 42 -1 0]
         ["abc" "xyz" "def" "jkl" "ghi"]])

(def int64-index [6 7 8 9 10])
(def double64-index [6.1 6.2 6.3 6.4 6.5])
(def bool-index [true false true false true])
(def obj-index ["m" "n" "o" "p" "q"])

(deftest dataframe-test
  (let [columns obj-index
        index int64-index
        df (dataframe vs :columns columns :index index)]
    (is (= [(count index) (count columns)] (:shape df)))
    (is (Arrays/equals (object-array columns)
                       (ndarray/tolist (index/to-numpy (:columns df)))))
    (is (Arrays/equals (long-array index) 
                       (ndarray/tolist (index/to-numpy (:index df)))))))

(deftest drop-test
  (let [columns obj-index
        index int64-index
        df (dataframe vs :columns columns :index index)]
    ;; drop first column
    (let [expected (dataframe (nthrest vs 1)
                              :columns (nthrest columns 1)
                              :index index)]
      (is (equals expected (drop* df (nth columns 0))))
      (is (equals expected (drop* df :columns (nth columns 0)))))

    ;; drop last two columns
    (let [n (- (count columns) 2)
          expected (dataframe (take n vs)
                              :columns (take n columns)
                              :index index)]
      (is (equals expected (drop* df (take-last 2 columns))))
      (is (equals expected (drop* df :columns (take-last 2 columns)))))

    ;; drop* first row
    (let [expected (dataframe (mapv #(nthrest % 1) vs)
                              :columns columns
                              :index (nthrest index 1))]
      (is (equals expected (drop* df (nth index 0) :axis 1)))
      (is (equals expected (drop* df :index (nth index 0)))))

    ;; drop last two rows
    (let [m (- (count index) 2)
          expected (dataframe (mapv #(take m %) vs)
                              :columns columns
                              :index (take m index))]
      (is (equals expected (drop* df (take-last 2 index) :axis 1)))
      (is (equals expected (drop* df :index (take-last 2 index)))))

    ;; drop last two columns and last two rows
    (let [m (- (count index) 2)
          n (- (count columns) 2)
          expected (dataframe (mapv #(take n %) (take m vs))
                              :columns (take m columns)
                              :index (take n index))]
      (is (equals expected (drop* df
                                 :columns (take-last 2 columns)
                                 :index (take-last 2 index)))))

    ;; drop labels and columns/index errors
    (let [expected #"^Cannot specify both 'labels' and 'index'/'columns'$"]
      (is (thrown-with-msg? Exception expected
                            (drop* df
                                  (nthrest vs 1)
                                  :columns (nthrest vs 1))))
      (is (thrown-with-msg? Exception expected
                            (drop* df
                                  (nthrest vs 1)
                                  :index (nthrest vs 1))))
      (is (thrown-with-msg? Exception expected
                            (drop* df
                                  (nthrest vs 1)
                                  :columns (nthrest vs 1)
                                  :index (nthrest vs 1)))))

    ;; drop label not found errors
    (let [expected #"^Not all column values can be found:"]
      (is (thrown-with-msg? Exception expected
                            (drop* df "not-found")))
      (is (thrown-with-msg? Exception expected
                            (drop* df :columns "not-found"))))
    
    (let [expected #"^Not all index values can be found:"]
      (is (thrown-with-msg? Exception expected
                            (drop* df "not-found" :axis 1)))
      (is (thrown-with-msg? Exception expected
                            (drop* df :index "not-found")))
      (is (thrown-with-msg? Exception expected
                            (drop* df :columns "not-found" 
                                   :index "not-found"))))

    ;; ignore not found errors
    (is (equals df (drop* df "not-found" :errors :ignore)))
    (is (equals df (drop* df :columns "not-found" :errors :ignore)))
    (is (equals df (drop* df "not-found" :axis 1 :errors :ignore)))
    (is (equals df (drop* df :index "not-found" :errors :ignore)))
    (is (equals df (drop* df :columns "not-found" :index "not-found" 
                          :errors :ignore)))))

