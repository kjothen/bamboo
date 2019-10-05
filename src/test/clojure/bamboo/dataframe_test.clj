(ns bamboo.dataframe-test
  (:refer-clojure :exclude [drop])
  (:require [clojure.test :refer :all]
            [bamboo.dataframe :refer :all]))

(deftest dataframe-tests
  (let [df (dataframe [[1 2 3 4] [5 6 7 8] [9 10 11 12]] :columns ["a" "b" "c"])]
    (is (= [4 3] (:shape df)))
    (is (= [4 2] (:shape (drop df ["b"]))))))

