(ns numcloj.utility)

(defn nan? {:tag Boolean} [x] (and (number? x) (Double/isNaN x)))
(defn not-nan? {:tag Boolean} [x] (not (nan? x)))
(defn nan= {:tag Boolean} [x y] (or (= x y) (every? nan? [x y])))

(defn array-zipmap
  "Returns a map with the `ks` mapped to the corresponding `vs` 
   preserving order"
  [ks vs]
  (apply array-map (interleave ks vs)))
