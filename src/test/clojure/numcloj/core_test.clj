(ns numcloj.core-test
  (:require [clojure.test :refer [deftest is]]
            [numcloj.core :refer [arange asarray delete empty* flatnonzero]]
            [numcloj.ndarray :refer [item]]))

;;; Array creation routines

;; Ones and zeros
(deftest zeros-and-ones-tests 
  (is (= :dtype/float64 (:dtype (empty* 3))))
  (is (= 3 (:size (asarray [1 2 3]))))
  (is (= 24998 (:size (delete (empty* 25000) [42 999])))))

;; Numerical ranges
(def arange-tests 
  (let [int64-range (arange 10 20 2)
        float64-range (arange 0.6 6.7 0.025)]
    (is (= 5 (:size int64-range)))
    (is (= 14 (item int64-range 2)))
    (is (= (long (Math/ceil (/ (- 6.7 0.6) 0.025))) (:size float64-range)))
    (is (= 6.625 (item float64-range (- (:size float64-range) 3))))
    ))

;;; Indexing routines
(deftest flatnonzero-test
  (is (= 2 (:size (flatnonzero (asarray [1 0 0 3]))))))

