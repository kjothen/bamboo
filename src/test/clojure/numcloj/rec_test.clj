(ns numcloj.rec-test
  (:require [clojure.test :refer [deftest is]]
            [numcloj.rec :refer [argsort fromarrays]]))

(deftest fromarrays-test
  (is (= [3 nil] (:shape (fromarrays [[1 2 3] [4 5 6]] 
                                     :names ["a" "b"])))))

(deftest argsort-test
  (let [a (fromarrays [[1 2 7 0] [1 2 6 1] [1 3 5 9]]
                      :names ["a" "b" "c"])]
    (is (= [4 nil] (:shape (argsort a))))))
