(ns numcloj.buffer
  (:import (java.util Arrays)))

;;;; an implementation of buffer based on java arrays

(defmulti make-buffer
  "Make a 1-D array of the right dtype"
  :dtype)
(defmethod make-buffer :default [a] (object-array (or (:seq a) (:size a))))
(defmethod make-buffer :dtype/bool [a] (boolean-array (or (:seq a) (:size a))))
(defmethod make-buffer :dtype/float64 [a] 
  (double-array 
   (if (contains? a :seq)
     (map (fnil identity ##NaN) (:seq a))
     (:size a))))
(defmethod make-buffer :dtype/int64 [a] (long-array (or (:seq a) (:size a))))

(defn buffer [dtype size] (make-buffer {:dtype dtype :size size}))
(defn from-sequential [dtype coll] (make-buffer {:dtype dtype :seq coll}))

(defn bcopy [buf] (Arrays/copyOf buf (alength buf)))
(defn bfill [buf val] (Arrays/fill buf val))
(defn bget [buf i] (aget buf i))
(defn bset [buf i x] (aset buf i x))
(defn bsize [buf] (alength buf))

(defn bequals [buf1 buf2] (Arrays/equals buf1 buf2))

