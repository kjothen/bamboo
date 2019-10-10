(ns numcloj.rec
  (:refer-clojure :exclude [empty])
  (:require [numcloj.api.array-manipulation :refer [copyto]]
            [numcloj.array-buffer :as b]
            [numcloj.array.conversion :refer [item]]
            [numcloj.array-creation :refer [asarray empty recarray]]
            [numcloj.utility :refer [array-zipmap]]))

(defn fromarrays
  "Create a record array from a (flat) list of arrays"
  [array-list & {:keys [dtype shape formats names titles aligned byteorder]
                 :or {aligned false}}]
  (let [as (mapv asarray array-list)
        a (recarray (or shape (:shape (first as)))
                    :format (or formats (mapv #(:dtype %) as))
                    :names names)
        m (first (:shape a))]
    (copyto a (asarray (map (fn [i] (array-zipmap names (map #(item % i) as)))
                            (range m))))
    a))

(defn argsort
  "Returns the indices that would sort an array"
  [a & {:keys [axis kind order]
        :or {axis -1 kind :stable}}]
  (let [len (:size a)
        indexed-a (empty len :dtype :dtype/object)
        dst (empty len :dtype :dtype/int64)
        ks (or order (:names a))
        comp-fn #(reduce (fn [_ k]
                           (let [x (get (b/get %1 1) k)
                                 y (get (b/get %2 1) k)
                                 res (compare x y)]
                             (if (neg? res) 
                               (reduced res)
                               res)))
                         0 ks)]
    (b/map-indexed-values #(:data (asarray [%1 %2])) a indexed-a)
    (b/sort-values (:data indexed-a) comp-fn)
    (b/map-values #(long (b/get % 0)) indexed-a dst)
    (asarray dst)))
