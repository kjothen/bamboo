(ns numcloj.api.array-manipulation-test
  (:refer-clojure :exclude [all any empty get])
  (:require [clojure.test :refer :all]
            [numcloj.test-utility :refer [as vs array-fixture
                                          random-samples invert-samples
                                          samples->mask ]]
            [numcloj.api.array-manipulation :refer :all]
            [numcloj.api.logic.comparison :refer [array-equal]]
            [numcloj.array.conversion :refer [item]]
            [numcloj.array-creation :refer [asarray copy empty]]
            [numcloj.utility :refer [nan=]]))

;; fixtures

(use-fixtures :each array-fixture)

;; tests

(deftest copyto-test
  (let [index (sort (random-samples))
        where (samples->mask index)
        dst-as (mapv (fn [[k v]] (empty (count index) :dtype k)) vs)
        expected (mapv (fn [[k v]] (asarray (mapv #(nth v %) index))) vs)]
    (doall (map-indexed
            (fn [i _]
              (copyto (nth dst-as i) (nth @as i) :where where)
              (is (array-equal (nth expected i) (nth dst-as i))))
            vs))))

(deftest delete-test
  (let [index (random-samples)
        inverse-index (invert-samples index)
        expected (mapv (fn [[k v]]
                         (asarray (mapv #(nth v %) inverse-index))) vs)]
    (doall (map-indexed
            (fn [i _]
              (is (array-equal (nth expected i) 
                               (delete (nth @as i) index))))
            vs))))
