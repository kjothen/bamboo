(ns numcloj.array.item-manipulation-test
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [numcloj.test-utility :refer [as array-fixture
                                          num-samples]]
            [numcloj.array.item-manipulation :refer [argsort]]))

;; fixtures

(use-fixtures :each array-fixture)

;; tests

(deftest argsort-tests
  (is (= num-samples (:size (argsort (nth @as 1) :order :ascending))))
  (is (= num-samples (:size (argsort (nth @as 1) :order :descending)))))