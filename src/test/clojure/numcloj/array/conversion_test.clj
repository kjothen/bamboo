(ns numcloj.array.conversion-test
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [numcloj.test-utility :refer [as array-fixture vs
                                          rand-sample random-values]]
            [numcloj.api.logic.comparison :as comparison]
            [numcloj.array-creation :refer [asarray empty*]]
            [numcloj.array.conversion :refer [copy fill item itemset]]
            [numcloj.utility :refer [nan=]]))

;; fixtures

(use-fixtures :each array-fixture)

;; tests

(deftest item-test
  (let [index (rand-sample)
        expected (mapv (fn [[_ v]] (nth v index)) vs)]
    (doall (map-indexed 
            (fn [i v] (is (nan= (nth expected i)
                                (item (nth @as i) index)))) 
            @as))))

(deftest itemset-test
  (let [index (rand-sample)
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
  (let [size (rand-sample)
        expected (random-values)]
    (doall (map-indexed
            (fn [i [k _]]
              (let [a (empty* size :dtype k)]
                (fill a (nth expected i))
                (is (comparison/array-equal
                     (asarray (repeat size (nth expected i)))
                     a))))
            vs))))
