(ns bamboo.dataframe-test
  (:require [clojure.pprint :as pprint]
            [clojure.test :refer [deftest is]]
            [io.aviso.ansi :as ansi]
            [bamboo.dataframe :refer [dataframe drop* equals
                                      loc sort-values to-string]]
            [bamboo.index :as index]
            [bamboo.series :as series]
            [numcloj.core :as np]
            [numcloj.traits]
            [numcloj.utility]))

(def as [[false true false true false]
         [99.0 -42.0 0.0 Double/NaN 16.66]
         [0 -1 42 99 23]
         [23 99 42 -1 0]
         ["abc" "xyz" "def" "jkl" "ghi"]])
(def tuples (apply map vector as))

(def int64-index [6 7 8 9 10])
(def double64-index [6.1 6.2 6.3 6.4 6.5])
(def bool-index [true false true false true])
(def obj-index ["m" "n" "o" "p" "q"])

(defn print-df [df] (println (to-string df)) (println))
(defn print-series [s] (println (series/to-string s)) (println))

(deftest dataframe-test
  (let [columns obj-index
        index int64-index
        df (dataframe tuples :columns columns :index index)]   
    (print-df df)
    
    ;; expected shape, columns and index 
    (is (= [(count index) (count columns)] (:shape df)))
    (is (np/array-equal columns (index/to-numpy (:columns df))))
    (is (np/array-equal index (index/to-numpy (:index df))))
    ))

(deftest loc-test
  (let [columns obj-index
        index int64-index
        df (dataframe tuples :columns columns :index index)]
    (println (ansi/bold-green "df"))
    (print-df df)

    ;; take single index
    (let [expected (series/series (first tuples) :index columns)]
      (println (ansi/green (format "(loc df %s)" (first index))))
      (print-series expected)
      (is (series/equals expected (loc df (first index)))))

    ;; take single index, two columns
    (let [expected (series/series (take 2 (first tuples))
                                  :index (take 2 columns))]
      (println (ansi/green (format "(loc df %s %s)"
                                   (first index) (vec (take 2 columns)))))
      (print-series expected)
      (is (series/equals expected (loc df (first index) (take 2 columns)))))

    ; take two indices
    (let [expected (dataframe (take 2 tuples)
                              :columns columns
                              :index (take 2 index))]
      (println (ansi/green (format "(loc df %s)" (vec (take 2 index)))))
      (print-df expected)
      (is (equals expected (loc df (take 2 index)))))

    ;; take two indices, two columns
    (let [expected (dataframe (map (partial take 2) (take 2 tuples))
                              :columns (take 2 columns)
                              :index (take 2 index))]
      (println (ansi/green (format "(loc df %s %s)"
                                   (vec (take 2 index))
                                   (vec (take 2 columns)))))
      (print-df expected)
      (is (equals expected (loc df (take 2 index) (take 2 columns)))))

    ;; TODO: ValueError, eg 2 columns passed, passed data had 5 columns
    ))
    
(deftest drop*-test
  (let [columns obj-index
        index int64-index
        df (dataframe tuples :columns columns :index index)]
    (println (ansi/bold-green "df"))
    (print-df df)

    ;; drop first column
    (let [expected (dataframe (apply map vector (nthrest as 1))
                              :columns (nthrest columns 1)
                              :index index)]
      (println (ansi/green (format "(drop* df \"%s\")"
                                   (nth columns 0))))
      (print-df expected)
      (is (equals expected (drop* df (nth columns 0))))
      (is (equals expected (drop* df :columns (nth columns 0)))))

    ;; drop last two columns
    (let [n (- (count columns) 2)
          expected (dataframe (apply map vector (take n as))
                              :columns (take n columns)
                              :index index)]
      (println (ansi/green (format "(drop* df %s)"
                                   (take-last 2 columns))))
      (print-df expected)
      (is (equals expected (drop* df (take-last 2 columns))))
      (is (equals expected (drop* df :columns (take-last 2 columns)))))

    ;; drop* first row
    (let [expected (dataframe (apply map vector (mapv #(nthrest % 1) as))
                              :columns columns
                              :index (nthrest index 1))]
      (println (ansi/green (format "(drop* df %d :axis 1)"
                                   (nth index 0))))
      (print-df expected)
      (is (equals expected (drop* df (nth index 0) :axis 1)))
      (is (equals expected (drop* df :index (nth index 0)))))

    ;; drop last two rows
    (let [m (- (count index) 2)
          expected (dataframe (apply map vector (mapv #(take m %) as))
                              :columns columns
                              :index (take m index))]
      (println (ansi/green (format "(drop* df %s :axis 1)"
                                   (take-last 2 index))))
      (print-df expected)
      (is (equals expected (drop* df (take-last 2 index) :axis 1)))
      (is (equals expected (drop* df :index (take-last 2 index)))))

    ;; drop last two columns and last two rows
    (let [m (- (count index) 2)
          n (- (count columns) 2)
          expected (dataframe (apply map vector (mapv #(take n %) (take m as)))
                              :columns (take m columns)
                              :index (take n index))]
      (println (ansi/green (format "(drop* df :index %s :columns %s)"
                                   (take-last 2 index)
                                   (take-last 2 columns))))
      (print-df expected)
      (is (equals expected (drop* df
                                  :index (take-last 2 index)
                                  :columns (take-last 2 columns)))))

    ;; drop under-specified errors
    (let [expected #"^Need to specify at least one of 'labels', 'index' or 'columns'"]
      (is (thrown-with-msg? Exception expected
                            (drop* df))))

    ;; drop labels and columns/index errors
    (let [expected #"^Cannot specify both 'labels' and 'index'/'columns'$"]
      (is (thrown-with-msg? Exception expected
                            (drop* df
                                   (nthrest as 1)
                                   :columns (nthrest as 1))))
      (is (thrown-with-msg? Exception expected
                            (drop* df
                                   (nthrest as 1)
                                   :index (nthrest as 1))))
      (is (thrown-with-msg? Exception expected
                            (drop* df
                                   (nthrest as 1)
                                   :columns (nthrest as 1)
                                   :index (nthrest as 1)))))

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
        df (dataframe tuples :columns columns :index index)]
    ;; sort by one column
    (println (ansi/bold-green "df"))
    (print-df df)
    (let [expected [[6 8 10 7 9]
                    [7 8 10 6 9]
                    [7 6 10 8 9]
                    [9 10 6 8 7]
                    [6 8 10 9 7]]]
      (doall
       (map (fn [idx]
              (let [df-sorted (sort-values df (nth columns idx))]
                (println (ansi/green
                          (format "(sort-values df \"%s\")" (nth columns idx))))
                (print-df df-sorted)
                (is (np/array-equal
                     (nth expected idx)
                     (index/to-numpy (:index df-sorted))))))
            (range (count columns)))))

    ;; sort by two columns    
    (let [expected [[8 10 6 7 9]
                    [7 8 10 6 9]
                    [7 6 10 8 9]
                    [9 10 6 8 7]]]
      (doall
       (map (fn [[idx1 idx2]]
              (let [df-sorted (sort-values df [(nth columns idx1)
                                               (nth columns idx2)])]
                (println (ansi/green
                          (format "(sort-values df [\"%s\", \"%s\"])"
                                  (nth columns idx1)
                                  (nth columns idx2))))
                (print-df df-sorted)
                (is (np/array-equal
                     (nth expected idx1)
                     (index/to-numpy (:index df-sorted))))))
            (partition 2 1 (range (count columns))))))

    ;; sort by all columns
    (let [df-sorted (sort-values df columns)
          expected [8 10 6 7 9]]
      (println (ansi/green (format "(sort-values df %s)" columns)))
      (print-df df-sorted)
      (is (np/array-equal
           expected
           (index/to-numpy (:index df-sorted)))))

    ;; sort by one index
    (with-redefs
     [numcloj.traits/*sort-kind* numcloj.utility/object-compare]
      (let [expected [["o","p","n","m","q"]
                      ["o","p","n","m","q"]
                      ["o","p","n","m","q"]
                      ["p","o","m","q","n"]
                      ["p","o","n","m","q"]]]
        (doall
         (map (fn [idx]
                (let [df-sorted (sort-values df (nth index idx) :axis 1)]
                  (println (ansi/green (format "(sort-values df %d :axis 1)"
                                               (nth index idx))))
                  (print-df df-sorted)
                  (is (np/array-equal
                       (nth expected idx)
                       (index/to-numpy (:columns df-sorted))))))
              (range (count index))))))

    ;; sort by two indices    
    (with-redefs
     [numcloj.traits/*sort-kind* numcloj.utility/object-compare]
      (let [expected [["o" "p" "n" "m" "q"]
                      ["o" "p" "n" "m" "q"]
                      ["p" "o" "n" "m" "q"]
                      ["p" "o" "m" "q" "n"]]]
        (doall
         (map
          (fn [[idx1 idx2]]
            (let [df-sorted (sort-values df [(nth index idx1)
                                             (nth index idx2)] :axis 1)]
              (println (ansi/green (format "(sort-values df [%d, %d] :axis 1)"
                                           (nth index idx1)
                                           (nth index idx2))))
              (print-df df-sorted)
              (is (np/array-equal
                   (nth expected idx1)
                   (index/to-numpy (:columns df-sorted))))))
          (partition 2 1 (range (count index)))))))

    ;; sort by all indices
    (with-redefs
     [numcloj.traits/*sort-kind* numcloj.utility/object-compare]
      (let [df-sorted (sort-values df index :axis 1)
            expected ["o" "p" "n" "m" "q"]]
        (println (ansi/green (format "(sort-values df %s :axis 1)" index)))
        (print-df df-sorted)
        (is (np/array-equal
             expected
             (index/to-numpy (:columns df-sorted))))))
    ))