(ns bamboo.dataframe-test
  (:refer-clojure :exclude [drop])
  (:require [clojure.test :refer :all]
            [bamboo.dataframe :refer :all]
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
    (is (equals (drop df (nth columns 0))
                (dataframe (nthrest vs 1)
                           :columns (nthrest columns 1)
                           :index index)))
    ;; drop last two columns
    (let [n (- (count vs) 2)]
      (is (equals (drop df (take-last 2 columns))
                  (dataframe (take n vs)
                             :columns (take n columns)
                             :index index))))))
