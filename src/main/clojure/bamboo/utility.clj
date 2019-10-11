(ns bamboo.utility)

(defmacro condas->
  "A mixture of cond-> and as-> allowing more flexibility in the test and step forms"
  [expr name & clauses]
  (assert (even? (count clauses)))
  (let [pstep (fn [[test step]] `(if ~test ~step ~name))]
    `(let [~name ~expr
           ~@(interleave (repeat name) (map pstep (partition 2 clauses)))]
       ~name)))

(defn nan? [x] (and (number? x) (Double/isNaN x)))
(defn not-nan? [x] (not (nan? x)))

(defn parse-long [s] (try (Long/parseLong s) (catch Exception e nil)))
(defn parse-double [s] (try (Double/parseDouble s) (catch Exception e nil)))
(defn parse-bool [s] (try (Boolean/parseBoolean s) (catch Exception e nil)))

(defn sum-vals 
  "Sum the values of the associative structure `m`, optionally
  ignoring the values for keys in `except-keys`"
  [m & {:keys [except-keys]}]
  (apply + (if (some? except-keys)
             (vals (apply dissoc m except-keys))
             (vals m))))

(defn update-many 
  "Apply `update` multiple times to an associative structure `m`
   where `ks` are keys and `fs` are functions"
  [m ks fs]
  (if-not (empty? ks)
    (update-many (update m (first ks) (first fs)) (rest ks) (rest fs))
    m))

(defn array-zipmap 
  "Returns a map with the `ks` mapped to the corresponding `vs` 
   preserving order"
  [ks vs]
  (apply array-map (interleave ks vs)))

(defn to-vector [x] (when (some? x) (if (coll? x) (vec x) (vector x))))

(defn in? [x coll] (if (some #{x} coll) true false))