(ns utility.core)

(defn atom? [x] (instance? clojure.lang.Atom x))

(defn front-back-split [len maximum & {:keys [minimum]}]
  (if (<= len maximum)
    {:indices (range len) :split nil}
    (if (= 1 maximum)
      {:indices (range 1) :split 1}
      (if (some? minimum)
        (let [_maximum (long (/ maximum 2))
              _minimum (min minimum _maximum)
              front (range _minimum)
              back (range (- len _minimum) len)]
          {:indices (concat front back) :split _minimum})
        (let [_maximum (long (/ maximum 2))
              front (range _maximum)
              back (range (- len _maximum) len)]
          {:indices (concat front back) :split _maximum})))))
