(ns numcloj.array-buffer-test
  (:refer-clojure :exclude [all any empty get])
  (:require [clojure.test :refer :all]
            [numcloj.test-utility :refer [vs as buffer-fixture num-samples 
                                          random-sample random-samples random-values]]
            [clojure.spec.gen.alpha :as sgen]
            [numcloj.utility :refer [nan=]]
            [numcloj.array-buffer :refer :all]))

;; fixtures

(use-fixtures :each buffer-fixture)

;; tests

(deftest buffer-constructors-test
  (is (= (Class/forName "[Z") (type (array :dtype/bool (random-sample)))))
  (is (= (Class/forName "[D") (type (array :dtype/float64 (random-sample)))))
  (is (= (Class/forName "[J") (type (array :dtype/int64 (random-sample)))))
  (is (= (Class/forName "[Ljava.lang.Object;") 
         (type (array :dtype/object (random-sample))))))

(deftest buffer-values-test
  (doall (map-indexed
          (fn [i [k v]]
            (doall (map-indexed
                    (fn [j vval]
                      (let [aval (get (nth @as i) j)]
                        (is (nan= vval aval))))
                    v)))
          vs)))

(deftest buffer-equals-test
  (doall (map #(is (equals % %)) @as))
  (doall (map #(is (= % %)) @as)))

(deftest buffer-copy-test
  (doall (map #(is (equals % (copy %))) @as))
  (doall (map #(is (not= % (copy %))) @as)))

(deftest buffer-set-test
  (let [index (random-sample)
        expected (random-values)]
    (doall (map-indexed
            (fn [i a]
              (set a index (nth expected i))
              (is (nan= (nth expected i) (get a index))))
           @as))))

(deftest buffer-fill-test
  (let [size (random-sample)
        expected (random-values)]
    (doall (map-indexed
            (fn [i [k v]]
              (let [a (array k size)]
                (fill a (nth expected i))
                (is (equals (from-sequential k (repeat size (nth expected i)))
                            a))))
            vs))))

(deftest map-values-test
  "Test map-values with the identity function as an alternative to copy"
  (let [dst-as (mapv (fn [[k v]] (array k (count v))) vs)
        to-array (fn [dtype data] {:dtype dtype :data data :size (size data)})]
    (doall
     (map-indexed
      (fn [i [k v]]
        (let [src (to-array k (nth @as i))
              dst (to-array k (nth dst-as i))]
          (is (equals (:data src) 
                      (map-values identity src dst)))))
      vs))))

(deftest keep-indexed-values-test
  "Test keep-indexed-values with the first n values"
  (let [n (random-sample)
        dst-as (mapv (fn [[k v]] (array k n)) vs)
        expected (mapv (fn [[k v]] (from-sequential k (take n v))) vs)
        f (fn [idx val] (< idx n))
        to-array (fn [dtype data] {:dtype dtype :data data :size (size data)})]
    (doall
     (map-indexed
      (fn [i [k v]]
        (let [src (to-array k (nth @as i))
              dst (to-array k (nth dst-as i))]
          (is (equals (nth expected i) 
                      (keep-indexed-values f src dst)))))
      vs))))

(deftest map-index-values-test
  "Test map-index-values-test with a sample index"
  (let [index (sort (random-samples))
        dst-as (mapv (fn [[k v]] (array k (count index))) vs)
        expected (mapv (fn [[k v]] (from-sequential k (mapv #(nth v %) index))) vs)
        to-array (fn [dtype data] {:dtype dtype :data data :size (size data)})]
    (doall
     (let [idx (to-array :dtype/int64 (from-sequential :dtype/int64 index))]
       (map-indexed
        (fn [i [k v]]
          (let [src (to-array k (nth @as i))
                dst (to-array k (nth dst-as i))]
            (is (equals (nth expected i)
                        (map-index-values idx src dst)))))
        vs)))))

(deftest assoc-index-test
  "TODO"
  (is true))
