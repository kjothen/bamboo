(ns numcloj.core-test
  (:require [clojure.test :refer [deftest is]]
            [numcloj.core :refer [asarray delete empty* flatnonzero]]))

(deftest zeros-and-ones-tests 
  (is (= :dtype/float64 (:dtype (empty* 3))))
  (is (= 3 (:size (asarray [1 2 3]))))
  (is (= 24998 (:size (delete (empty* 25000) [42 999])))))

(deftest flatnonzero-test
  (is (= 2 (:size (flatnonzero (asarray [1 0 0 3]))))))
