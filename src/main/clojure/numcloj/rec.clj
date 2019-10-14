(ns numcloj.rec
  (:require [numcloj.api.array-manipulation :refer [copyto]]
            [numcloj.array-buffer :as b]
            [numcloj.array.conversion :refer [item]]
            [numcloj.array-creation :refer [asarray empty* recarray]]
            [numcloj.utility :refer [array-zipmap]]))

;; an ndarray of objects, where each object is a clojure array-map

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
        ks (or order (:names a))
        columns (zipmap (range len) (:data a))
        keyfn (fn [m] (vec (vals (select-keys (second m) ks))))
        result (map first (sort-by keyfn columns))]
    (asarray (b/from-sequential :dtype/int64 result))))