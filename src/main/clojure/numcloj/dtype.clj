(ns numcloj.dtype)

(def numcloj-hierarchy (-> (make-hierarchy)
                           (derive :dtype/int64 :dtype/number)
                           (derive :dtype/float64 :dtype/number)

                           (derive :dtype/bool :dtype/numcloj)
                           (derive :dtype/int64 :dtype/numcloj)
                           (derive :dtype/float64 :dtype/numcloj)
                           (derive :dtype/object :dtype/numcloj)
                           (derive :dtype/record :dtype/numcloj)))

(defn- sum-vals
  "Sum the values of the associative structure `m`, optionally
  ignoring the values for keys in `except-keys`"
  [m & {:keys [except-keys]}]
  (apply + (if (some? except-keys)
             (vals (apply dissoc m except-keys))
             (vals m))))

(defn- scan-dtypes
  "Map each item type to its corresponding dtype in a collection `coll`,
   returning their associated frequencies, 
   or an error if no equivalent dtype exists"
  [coll]
  (reduce (fn [acc item]
            (condp = (type item)
              java.lang.Boolean (update acc :dtype/bool inc)
              java.lang.Float (update acc :dtype/float64 inc)
              java.lang.Double (update acc :dtype/float64 inc)
              java.lang.Integer (update acc :dtype/int64 inc)
              java.lang.Long (update acc :dtype/int64 inc)
              java.lang.String (update acc :dtype/object inc)
              clojure.lang.PersistentArrayMap (update acc :dtype/object inc) ; for records
              clojure.lang.Keyword (update acc :keyword inc)
              nil (update acc :nil inc)
              (reduced (assoc acc :error (inc (sum-vals acc))))))
          {:dtype/bool 0 :dtype/float64 0 :dtype/int64 0
           :dtype/object 0 :dtype/record 0 :nil 0 :keyword 0} coll))

(defn from-frequencies
  "Given a frequency map of `dtypes` for a collection, return the most
   appropriate `dtype` for the entire collection."
  [dtypes]
  (cond
    ; errors
    (contains? dtypes :error) :error
    ; objects
    (pos? (:dtype/object dtypes)) :dtype/object
    ; keywords
    (pos? (:keyword dtypes)) :dtype/object
    ; floats (and optionally ints and nils)
    (pos? (:dtype/float64 dtypes))
    (if (pos? (sum-vals dtypes :except-keys
                        [:nil :dtype/float64 :dtype/int64]))
      :dtype/object
      :dtype/float64)
    ; ints
    (pos? (:dtype/int64 dtypes))
    (if (pos? (sum-vals dtypes :except-keys [:dtype/int64]))
      (if (pos? (sum-vals dtypes :except-keys
                          [:nil :dtype/float64 :dtype/int64]))
        :dtype/object
        :dtype/float64)
      :dtype/int64)
    ; bools
    (pos? (:dtype/bool dtypes))
    (if (pos? (sum-vals dtypes :except-keys [:dtype/bool]))
      :dtype/object
      :dtype/bool)
    :else :dtype/object))

(defn infer-dtype [coll] (from-frequencies (scan-dtypes coll)))

