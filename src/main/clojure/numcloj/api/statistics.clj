(ns numcloj.api.statistics
  (:require [numcloj.array-buffer :as b]
            [numcloj.array-creation :refer [asarray]]
            [numcloj.array.conversion :refer [item]]))

(defn amax
  "Return the maximum of an array or maximum along an axis"
  [a & {:keys [axis out keepdims initial where]}]
  (let [_a (asarray a)
        f #(max %1 %2)]
    (b/reduce* f _a (item _a 0))))

