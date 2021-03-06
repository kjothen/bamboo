(ns numcloj.array-buffer
  (:import (java.util Arrays)))

;;;; an implementation of buffer based on java arrays

(defmulti ^:private make-buffer
  "Make a 1-D array of the right dtype"
  :dtype)
(defmethod make-buffer :default [a] (object-array (or (:seq a) (:size a))))
(defmethod make-buffer :dtype/bool [a] (boolean-array (or (:seq a) (:size a))))
(defmethod make-buffer :dtype/float64 [a] 
  (double-array 
   (if (contains? a :seq)
     (map (fnil identity Double/NaN) (:seq a))
     (:size a))))
(defmethod make-buffer :dtype/int64 [a] (long-array (or (:seq a) (:size a))))

(defmulti dtype class)
(defmethod dtype :default [_] :dtype/object)
(defmethod dtype (Class/forName "[Z") [_] :dtype/bool)
(defmethod dtype (Class/forName "[D") [_] :dtype/float64)
(defmethod dtype (Class/forName "[J") [_] :dtype/int64)
(defmethod dtype (Class/forName "[Ljava.lang.Object;") [_] :dtype/object)

(defn array [dtype size] (make-buffer {:dtype dtype :size size}))
(defn from-sequential [dtype coll] (make-buffer {:dtype dtype :seq coll}))

(defn copy
  ([buf] (Arrays/copyOf buf (alength buf)))
  ([buf len] (Arrays/copyOf buf len)))
(defn fill [buf val#] (Arrays/fill buf val#))
(def get* aget)
(def set* aset)
(def size alength)

(defn tolist [a] a)

(defn equals [buf1 buf2] (Arrays/equals buf1 buf2))

(defn sort-values 
  ([buf] (Arrays/sort buf))
  ([buf c] (Arrays/sort buf c)))

(defn reduce*
  [f data init]
  (areduce (:data data) idx ret init (f ret (get* (:data data) idx))))

(defn- type-hint-array-fn
  "Provide type hinting to higher-performance expressions"
  ([array-fn f data]
   (case (:dtype data)
     :dtype/bool (partial array-fn ^Boolean f ^booleans (:data data))
     :dtype/float64 (partial array-fn ^Double f ^doubles (:data data))
     :dtype/int64 (partial array-fn ^Long f ^longs (:data data))
     (partial array-fn f (:data data))))

  ([array-fn f src-data dst-data]
   (let [pfn (type-hint-array-fn array-fn f dst-data)]
     (case (:dtype src-data)
       :dtype/bool (partial pfn ^booleans (:data src-data))
       :dtype/float64 (partial pfn ^doubles (:data src-data))
       :dtype/int64 (partial pfn ^longs (:data src-data))
       (partial pfn (:data src-data)))))

  ([array-fn f idx-data src-data dst-data]
   (let [pfn (type-hint-array-fn array-fn f dst-data idx-data)]
     (case (:dtype src-data)
       :dtype/bool (partial pfn ^booleans (:data src-data))
       :dtype/float64 (partial pfn ^doubles (:data src-data))
       :dtype/int64 (partial pfn ^longs (:data src-data))
       (partial pfn (:data src-data))))))

(defn map-values
  "A higher-performance equivalent to `(into dst (map #(f %1) src))`
   for buffers `src` and `dst`, where `dst` is of a fixed size"
  [f src dst]
  (let [afn (fn [f dst src]
              (let [ls (size src)
                    ld (size dst)]
                (loop [i 0]
                  (if (and (< i ls) (< i ld))
                    (let [val (get* src i)
                          fval (f val)]
                      (set* dst i fval)
                      (recur (unchecked-inc i)))
                    dst))))
        typed-afn (type-hint-array-fn afn f src dst)]
    (typed-afn)))

(defn reverse-values
  "A higher-performance equivalent to `(into dst (reverse src))`
   for buffers `src` and `dst` of equal size"
  [src dst]
  (let [afn (fn [_ dst src]
              (let [ls (size src)
                    ld (size dst)]
                (loop [i 0
                       j (dec ld)]
                  (if (and (< i ls) (>= j 0))
                    (do
                      (set* dst j (get* src i))
                      (recur (unchecked-inc i) (unchecked-dec j)))
                    dst))))
        typed-afn (type-hint-array-fn afn identity src dst)]
    (typed-afn)))

(defn dual-map-values
  "A higher-performance equivalent to 
  `(into dst (map #(f (first %) (second %)) (partition 2 (interleave src1 src2))))`
   for buffers `src1`, `src2` and `dst`, where `dst` is of a fixed size"
  [f src1 src2 dst]
  (let [afn (fn [_ src1 dst src2]
              (let [ilast (dec (size src1))
                    jlast (dec (size src2))
                    klen (size dst)]
                (loop [i 0
                       j 0
                       k 0]
                  (if (< k klen)
                    (let [val1 (get* src1 i)
                          val2 (get* src2 j)
                          fval (f val1 val2)]
                      (set* dst k fval)
                      (recur (if (< i ilast) (unchecked-inc i) ilast)
                             (if (< j jlast) (unchecked-inc j) jlast)
                             (unchecked-inc k)))
                    dst))))
        typed-afn (type-hint-array-fn afn f src1 src2 dst)]
    (typed-afn)))

(defn map-indexed-values
  "A higher-performance equivalent to `(into dst (map-indexed #(f %1 %2) src))`
   for buffers `src` and `dst`, where `dst` is of a fixed size"
  [f src dst]
  (let [afn (fn [f dst src]
              (let [ls (size src)
                    ld (size dst)]
                (loop [i 0]
                  (if (and (< i ls) (< i ld))
                    (let [val (get* src i)
                          fval (f i val)]
                      (set* dst i fval)
                      (recur (unchecked-inc i)))
                    dst))))
        typed-afn (type-hint-array-fn afn f src dst)]
    (typed-afn)))

(defn map-index-values
  "A higher-performance equivalent to `(into dst (map #(nth src %) index))`
   for buffers `src`, `dst` and `index`, where `dst` is of a fixed size"
  [index src dst]
  (let [afn (fn [_ index dst src]
              (let [li (size index)
                    ld (size dst)]
                (loop [i 0]
                  (if (and (< i li) (< i ld))
                    (let [idx (get* index i)
                          val (get* src idx)]
                      (set* dst i val)
                      (recur (unchecked-inc i)))
                    dst))))
        typed-afn (type-hint-array-fn afn identity index src dst)]
    (typed-afn)))

(defn keep-indexed-indices
  "A higher-performance equivalent to 
   `(into dst (keep-indexed #(when #(f %2) %1) src))`
   for buffers `src` and `dst`, where `dst` has a fixed size"
  [f src dst]
  (let [afn (fn [f dst src]
              (let [ls (size src)
                    ld (size dst)]
                (loop [i 0
                       j 0]
                  (if (and (< i ls) (< j ld))
                    (let [val (get* src i)
                          keep? (f i val)]
                      (if keep? (set* dst j i))
                      (recur (unchecked-inc i)
                             (if keep? (unchecked-inc j) j)))
                    dst))))
        typed-afn (type-hint-array-fn afn f src dst)]
    (typed-afn)))

(defn keep-indexed-values
  "A higher-performance equivalent to 
   `(into dst (keep-indexed #(when #(f %2) %2) src))`
   for buffers `src` and `dst`, where `dst` has a fixed size"
  [f src dst]
  (let [afn (fn [f dst src]
              (let [ls (size src)
                    ld (size dst)]
                (loop [i 0
                       j 0]
                  (if (and (< i ls) (< j ld))
                    (let [val (get* src i)
                          keep? (f i val)]
                      (if keep? (set* dst j val))
                      (recur (unchecked-inc i)
                             (if keep? (unchecked-inc j) j)))
                    dst))))
        typed-afn (type-hint-array-fn afn f src dst)]
    (typed-afn)))

(defn assoc-index
  "A higher-performance equivalent to 
   `(reduce #(assoc %1 %2 #(nth src %2)) dst index)`
   for buffers `src`, `dst` and `index`, where `dst` is of a fixed size"
  [index src dst]
  (let [afn (fn [_ index dst src]
              (let [li (size index)
                    ls (size src)]
                (loop [i 0]
                  (if (and (< i li) (< i ls))
                    (let [idx (get* index i)
                          val (get* src i)]
                      (set* dst idx val)
                      (recur (unchecked-inc i)))
                    dst))))
        typed-afn (type-hint-array-fn afn identity index src dst)]
    (typed-afn)))

(defn arange
  "A higher-performance equivalent to `(vec (range start stop step))`
   but truncates results to the number of decimal places in `(+ start step)`"
  [start stop step dtype]
  (case dtype
    :dtype/int64 (let [len (long (/ (- stop start) step))
                       dst (array dtype len)]
                   (loop [i 0]
                     (if (< i len)
                       (do
                         (aset-long dst i (+ start (* step i)))
                         (recur (unchecked-inc i)))
                       dst)))
    :dtype/float64 (let [len (Math/ceil (/ (- stop start) step))
                         dps (loop [n 1] 
                               (let [r (Math/IEEEremainder 
                                        (* (+ start step) (Math/pow 10 n))
                                        1.0)]
                                 (if-not (or (zero? r) (>= n 16))
                                   (recur (unchecked-inc n))
                                   n)))
                         scale (Math/pow 10 dps)
                         dst (array dtype len)]
                     (loop [i 0]
                       (if (< i len)
                         (let [val (+ start (* step i))]
                           (aset-double dst i
                                        (/ (Math/round (* val scale)) scale))
                           (recur (unchecked-inc i)))
                         dst)))))
