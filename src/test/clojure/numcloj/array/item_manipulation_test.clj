(ns numcloj.array.item-manipulation-test
  (:require [clojure.test :refer :all]
            [numcloj.test-utility :refer [as array-fixture vs
                                          num-samples]]
            [numcloj.api.logic.comparison :as comparison]
            [numcloj.array-creation :refer [asarray empty]]
            [numcloj.array.item-manipulation :refer :all]
            [numcloj.utility :refer [nan=]]))

;; fixtures

(use-fixtures :each array-fixture)

;; tests

(deftest argsort-tests
  (is (= num-samples (:size (argsort (nth @as 1) :order :ascending))))
  (is (= num-samples (:size (argsort (nth @as 1) :order :descending)))))