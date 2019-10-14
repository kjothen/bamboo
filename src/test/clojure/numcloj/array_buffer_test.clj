(ns numcloj.array-buffer-test
  (:refer-clojure :exclude [all any])
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [numcloj.test-utility :refer [vs as buffer-fixture num-samples 
                                          rand-sample rand-samples 
                                          random-values]]
            [numcloj.utility :refer [nan=]]
            [numcloj.array-buffer :as b]))

;; fixtures

(use-fixtures :each buffer-fixture)

;; tests

(deftest buffer-constructors-test
  (is (= (Class/forName "[Z") (type (b/array :dtype/bool (rand-sample)))))
  (is (= (Class/forName "[D") (type (b/array :dtype/float64 (rand-sample)))))
  (is (= (Class/forName "[J") (type (b/array :dtype/int64 (rand-sample)))))
  (is (= (Class/forName "[Ljava.lang.Object;") 
         (type (b/array :dtype/object (rand-sample))))))

(deftest buffer-values-test
  (doall (map-indexed
          (fn [i [_ v]]
            (doall (map-indexed
                    (fn [j vval]
                      (let [aval (b/get* (nth @as i) j)]
                        (is (nan= vval aval))))
                    v)))
          vs)))

(deftest buffer-equals-test
  (doall (map #(is (b/equals % %)) @as))
  (doall (map #(is (= % %)) @as)))

(deftest buffer-copy-test
  (doall (map #(is (b/equals % (b/copy %))) @as))
  (doall (map #(is (not= % (b/copy %))) @as)))

(deftest buffer-set-test
  (let [index (rand-sample)
        expected (random-values)]
    (doall (map-indexed
            (fn [i a]
              (b/set* a index (nth expected i))
              (is (nan= (nth expected i) (b/get* a index))))
           @as))))

(deftest buffer-fill-test
  (let [size (rand-sample)
        expected (random-values)]
    (doall (map-indexed
            (fn [i [k _]]
              (let [a (b/array k size)]
                (b/fill a (nth expected i))
                (is (b/equals 
                     (b/from-sequential k (repeat size (nth expected i)))
                     a))))
            vs))))

(deftest map-values-test
  "Test map-values with the identity function as an alternative to copy"
  (let [dst-as (mapv (fn [[k v]] (b/array k (count v))) vs)
        to-array (fn [dtype data] 
                   {:dtype dtype :data data :size (b/size data)})]
    (doall
     (map-indexed
      (fn [i [k _]]
        (let [src (to-array k (nth @as i))
              dst (to-array k (nth dst-as i))]
          (is (b/equals (:data src) 
                      (b/map-values identity src dst)))))
      vs))))

(deftest keep-indexed-values-test
  "Test keep-indexed-values with the first n values"
  (let [n (rand-sample)
        dst-as (mapv (fn [[k _]] (b/array k n)) vs)
        expected (mapv (fn [[k v]] (b/from-sequential k (take n v))) vs)
        f (fn [idx val] (< idx n))
        to-array (fn [dtype data] 
                   {:dtype dtype :data data :size (b/size data)})]
    (doall
     (map-indexed
      (fn [i [k _]]
        (let [src (to-array k (nth @as i))
              dst (to-array k (nth dst-as i))]
          (is (b/equals (nth expected i) 
                      (b/keep-indexed-values f src dst)))))
      vs))))

(deftest map-index-values-test
  "Test map-index-values-test with a sample index"
  (let [index (sort (rand-samples))
        dst-as (mapv (fn [[k _]] (b/array k (count index))) vs)
        expected (mapv (fn [[k v]] 
                         (b/from-sequential k (mapv #(nth v %) index))) vs)
        to-array (fn [dtype data] 
                   {:dtype dtype :data data :size (b/size data)})]
    (doall
     (let [idx (to-array :dtype/int64 (b/from-sequential :dtype/int64 index))]
       (map-indexed
        (fn [i [k _]]
          (let [src (to-array k (nth @as i))
                dst (to-array k (nth dst-as i))]
            (is (b/equals (nth expected i)
                        (b/map-index-values idx src dst)))))
        vs)))))

(deftest assoc-index-test
  "TODO"
  (is true))
