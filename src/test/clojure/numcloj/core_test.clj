(ns numcloj.core-test
  (:refer-clojure :exclude [all any empty])
  (:require [clojure.test :refer :all]
            [numcloj.core :refer :all]))

(deftest zeros-and-ones-tests 
  (is (= :dtype/float64 (empty 3))))
