(ns numcloj.array.conversion-test
  (:require [clojure.test :refer :all]
            [numcloj.test-utility :refer [as array-fixture vs
                                          random-sample random-values]]
            [numcloj.api.logic.comparison :as comparison]
            [numcloj.array-creation :refer [asarray empty]]
            [numcloj.array.conversion :refer :all]
            [numcloj.utility :refer [nan=]]))

;; fixtures

(use-fixtures :each array-fixture)

;; tests

(deftest item-test
  (let [index (random-sample)
        expected (mapv (fn [[k v]] (nth v index)) vs)]
    (doall (map-indexed 
            (fn [i v] (is (nan= (nth expected i)
                                (item (nth @as i) index)))) 
            @as))))

(deftest itemset-test
  (let [index (random-sample)
        expected (random-values)]
    (doall (map-indexed
            (fn [i a]
              (itemset a index (nth expected i))
              (is (nan= (nth expected i) (item a index))))
            @as))))

(deftest copy-test
  (let [expected (mapv copy @as)]
    (doall (map-indexed
            (fn [i a]
              (is (comparison/array-equal (nth expected i) a)))
            @as))))

(deftest fill-test
  (let [size (random-sample)
        expected (random-values)]
    (doall (map-indexed
            (fn [i [k v]]
              (let [a (empty size :dtype k)]
                (fill a (nth expected i))
                (is (comparison/array-equal
                     (asarray (repeat size (nth expected i)))
                     a))))
            vs))))
