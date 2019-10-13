(ns numcloj.api.array-manipulation-test
  (:refer-clojure :exclude [all any])
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [numcloj.test-utility :refer [as vs array-fixture
                                          rand-samples invert-samples
                                          samples->mask ]]
            [numcloj.api.array-manipulation :refer [copyto delete]]
            [numcloj.api.logic.comparison :refer [array-equal]]
            [numcloj.array-creation :refer [asarray copy empty*]]))

;; fixtures

(use-fixtures :each array-fixture)

;; tests

(deftest copyto-test
  (let [index (sort (rand-samples))
        where (samples->mask index)
        dst-as (mapv (fn [[k _]] (empty* (count index) :dtype k)) vs)
        expected (mapv (fn [[_ v]] (asarray (mapv #(nth v %) index))) vs)]
    (doall (map-indexed
            (fn [i _]
              (copyto (nth dst-as i) (nth @as i) :where where)
              (is (array-equal (nth expected i) (nth dst-as i))))
            vs))))

(deftest delete-test
  (let [index (rand-samples)
        inverse-index (invert-samples index)
        expected (mapv (fn [[_ v]]
                         (asarray (mapv #(nth v %) inverse-index))) vs)]
    (doall (map-indexed
            (fn [i _]
              (is (array-equal (nth expected i) 
                               (delete (nth @as i) index))))
            vs))))
