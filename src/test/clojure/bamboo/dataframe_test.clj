(ns bamboo.dataframe-test
  (:require [clojure.test :refer [deftest is]]
            [bamboo.dataframe :refer [dataframe drop* equals loc sort-values 
                                      to-string]]
            [bamboo.index :as index]
            [numcloj.core :as np]))

(def vs [[false true false true false]
         [99.0 -42.0 0.0 Double/NaN 16.66]
         [0 -1 42 99 23]
         [23 99 42 -1 0]
         ["abc" "xyz" "def" "jkl" "ghi"]])

(def int64-index [6 7 8 9 10])
(def double64-index [6.1 6.2 6.3 6.4 6.5])
(def bool-index [true false true false true])
(def obj-index ["m" "n" "o" "p" "q"])

(defn print-df [df] (do (println) (println (to-string df))))

(deftest dataframe-test
  (let [columns obj-index
        index int64-index
        df (dataframe vs :columns columns :index index)]   
    (print-df df)
    
    ;; expected shape, columns and index 
    (is (= [(count index) (count columns)] (:shape df)))
    (is (np/array-equal columns (index/to-numpy (:columns df))))
    (is (np/array-equal index (index/to-numpy (:index df))))))

(deftest loc-test
  (let [columns obj-index
        index int64-index
        df (dataframe vs :columns columns :index index)]
    (print-df df)

    ;; take single index
    (let [expected (dataframe (map #(take 1 %) vs)
                              :columns columns
                              :index (take 1 index))]
      (print-df df)
      (is (equals expected (loc df (first index)))))

    ;; take multiple indices
    (let [expected (dataframe (map #(take 2 %) vs)
                              :columns columns
                              :index (take 2 index))]
      (print-df df)
      (is (equals expected (loc df (take 2 index)))))
    
    ;; take multiple index, mutliple columns
    (let [expected (dataframe (map #(take 1 %) (take 2 vs))
                              :columns (take 2 columns)
                              :index (take 1 index))]
      (print-df expected)
      (is (equals expected (loc df (first index) (take 2 columns)))))
    
    ;; take multiple indices, multiple columns
    (let [expected (dataframe (map #(take 2 %) (take 2 vs))
                              :columns (take 2 columns)
                              :index (take 2 index))]
      (print-df expected)
      (is (equals expected (loc df (take 2 index) (take 2 columns)))))
    ))
    
(deftest drop*-test
  (let [columns obj-index
        index int64-index
        df (dataframe vs :columns columns :index index)]
    (print-df df)

    ;; drop first column
    (let [expected (dataframe (nthrest vs 1)
                              :columns (nthrest columns 1)
                              :index index)]
      (print-df expected)
      (is (equals expected (drop* df (nth columns 0))))
      (is (equals expected (drop* df :columns (nth columns 0)))))

    ;; drop last two columns
    (let [n (- (count columns) 2)
          expected (dataframe (take n vs)
                              :columns (take n columns)
                              :index index)]
      (print-df expected)
      (is (equals expected (drop* df (take-last 2 columns))))
      (is (equals expected (drop* df :columns (take-last 2 columns)))))
    
    ;; drop* first row
    (let [expected (dataframe (mapv #(nthrest % 1) vs)
                              :columns columns
                              :index (nthrest index 1))]
      (print-df expected)
      (is (equals expected (drop* df (nth index 0) :axis 1)))
      (is (equals expected (drop* df :index (nth index 0)))))
    
    ;; drop last two rows
    (let [m (- (count index) 2)
          expected (dataframe (mapv #(take m %) vs)
                              :columns columns
                              :index (take m index))]
      (print-df expected)
      (is (equals expected (drop* df (take-last 2 index) :axis 1)))
      (is (equals expected (drop* df :index (take-last 2 index)))))
    
    ;; drop last two columns and last two rows
    (let [m (- (count index) 2)
          n (- (count columns) 2)
          expected (dataframe (mapv #(take n %) (take m vs))
                              :columns (take m columns)
                              :index (take n index))]
      (print-df expected)
      (is (equals expected (drop* df
                                  :columns (take-last 2 columns)
                                  :index (take-last 2 index)))))
    
    ;; drop under-specified errors
    (let [expected #"^Need to specify at least one of 'labels', 'index' or 'columns'"]
      (is (thrown-with-msg? Exception expected
                            (drop* df))))
    
    ;; drop labels and columns/index errors
    (let [expected #"^Cannot specify both 'labels' and 'index'/'columns'$"]
      (is (thrown-with-msg? Exception expected
                            (drop* df
                                   (nthrest vs 1)
                                   :columns (nthrest vs 1))))
      (is (thrown-with-msg? Exception expected
                            (drop* df
                                   (nthrest vs 1)
                                   :index (nthrest vs 1))))
      (is (thrown-with-msg? Exception expected
                            (drop* df
                                   (nthrest vs 1)
                                   :columns (nthrest vs 1)
                                   :index (nthrest vs 1)))))
    
    ;; drop label not found errors
    (let [expected #"^\[not-found\] not found in axis"]
      (is (thrown-with-msg? Exception expected
                            (drop* df "not-found")))
      (is (thrown-with-msg? Exception expected
                            (drop* df :columns "not-found"))))
    
    (let [expected #"^\[not-found\] not found in axis"]
      (is (thrown-with-msg? Exception expected
                            (drop* df "not-found" :axis 1)))
      (is (thrown-with-msg? Exception expected
                            (drop* df :index "not-found")))
      (is (thrown-with-msg? Exception expected
                            (drop* df :columns "not-found"
                                   :index "not-found"))))
    
    ;; ignore not found errors
    (print-df df)
    (is (equals df (drop* df "not-found" :errors :ignore)))
    (is (equals df (drop* df :columns "not-found" :errors :ignore)))
    (is (equals df (drop* df "not-found" :axis 1 :errors :ignore)))
    (is (equals df (drop* df :index "not-found" :errors :ignore)))
    (is (equals df (drop* df :columns "not-found" :index "not-found"
                          :errors :ignore)))
    ))

(deftest sort-values-test
  (let [columns obj-index
        index int64-index
        df (dataframe vs :columns columns :index index)]

    ;; sort by one column
    (print-df df)
    (doall
     (map-indexed
      (fn [idx v]
        (let [df-sorted (sort-values df (nth columns idx))
              expected (map first (sort-by second compare (zipmap index v)))]
          (print-df df-sorted)
          (is (np/array-equal
               expected
               (index/to-numpy (:index df-sorted))))))
      vs))

    ;; sort by two columns    
    (print-df df)
    (doall
     (map
      (fn [[idx1 idx2]]
        (let [df-sorted (sort-values df [(nth columns idx1)
                                         (nth columns idx2)])
              columns (zipmap index 
                              (partition 2 (interleave (nth vs idx1)
                                                       (nth vs idx2))))
              expected (map first (sort-by
                                   (juxt #(nth (second %) 0)
                                         #(nth (second %) 1))
                                   columns))]
          (print-df df-sorted)
          (is (np/array-equal
               expected
               (index/to-numpy (:index df-sorted))))))
      (partition 2 1 (range (count columns)))))
    ))
