(ns numcloj.utility)

(defn nan? {:tag Boolean} [x] (and (number? x) (Double/isNaN x)))
(defn not-nan? {:tag Boolean} [x] (not (nan? x)))
(defn nan= {:tag Boolean} [x y] (or (= x y) (every? nan? [x y])))

(defn array-zipmap
  "Returns a map with the `ks` mapped to the corresponding `vs` 
   preserving order"
  [ks vs]
  (apply array-map (interleave ks vs)))

(defn object-compare
  "Compares two objects of any type. The sort order of types is:
   Number, Boolean, String, Other, nil/##NaN"
  [x y]
  (let [xnil? (or (nan? x) (nil? x))
        ynil? (or (nan? y) (nil? y))]
    (cond
      (and xnil? ynil?) 0
      xnil? 1
      ynil? -1
      (= (type x) (type y)) (compare x y)
      :else (condp instance? x
              Boolean (if (number? y) 1 -1)
              Number -1
              1))))