(ns numcloj.api.counting
  (:require [numcloj.array-creation :refer [asarray]]
            [numcloj.array-buffer :as b]))

(defn count-nonzero
  "Counts the number of non-zero values in the array a"
  [a & {:keys [axis]}]
  (let [_a (asarray a)
        f (if (= :dtype/bool (:dtype _a))
            #(if (false? %) 0 1)
            #(if (zero? %) 0 1))]
    (b/reduce f _a (long 0))))

