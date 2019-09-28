(ns numcloj.creation
  (:refer-clojure :exclude [empty])
  (:require [numcloj.buffer :as buffer]
            [numcloj.array.conversion :as conversion]
            [numcloj.dtype :as dtype]))

;;;; Array creation routines
;;;; https://docs.scipy.org/doc/numpy/reference/routines.array-creation.html

;;; helpers
(defmulti shaper class)
(defmethod shaper :default [shape] shape)
(defmethod shaper Number [shape] [(long shape) nil])

;;; ndarray
(defn- _size [data] (buffer/bsize data))
(defn- _itemsize [data] (* 8 (_size data)))
(defn- _ndim [data] 1)
(defn- _nbytes [data] (* (_ndim data) (_itemsize data)))
(defn- _strides [data] [(_itemsize data) nil])

;;; ndarray
(defn- ndarray
  [shape dtype & {:keys [buffer offset strides order]}]
  (let [_shape (shaper shape)
        data (or buffer (buffer/buffer dtype (first _shape)))]
    {:data data
     :dtype dtype
     :shape _shape
     :ndim (_ndim data)
     :size (_size data)
     :itemsize (_itemsize data)
     :strides (or strides (_strides data))
     :nbytes (_nbytes data)
     :flags {:writebackifcopy false :updateifcopy false
             :aligned false :writeable true}}))

(defn ndarray? [a]
  (and (map? a) (isa? dtype/numcloj-hierarchy (:dtype a) :dtype/numcloj)))

;;; From existing data

;; https://docs.scipy.org/doc/numpy/reference/generated/numpy.asarray.html#numpy.asarray
(defn- asclojurearray [a]
  (let [dtype (dtype/infer-dtype a)]
    (ndarray (count a) dtype :buffer (buffer/from-sequential dtype a))))
(defmulti asarray
  "Convert the input to an array"
  class)
(defmethod asarray :default [a] a)
(defmethod asarray (Class/forName "[Z") [a] (ndarray (alength a) :dtype/bool :buffer a))
(defmethod asarray (Class/forName "[D") [a] (ndarray (alength a) :dtype/double :buffer a))
(defmethod asarray (Class/forName "[J") [a] (ndarray (alength a) :dtype/long :buffer a))
(defmethod asarray clojure.lang.PersistentVector [a] (asclojurearray a))
(defmethod asarray clojure.lang.PersistentList [a] (asclojurearray a))
(defmethod asarray clojure.lang.LazySeq [a] (asclojurearray (vec a)))

;; https://docs.scipy.org/doc/numpy/reference/generated/numpy.array.html#numpy.array
(defn array
  "Create an array"
  [a & {:keys [dtype copy order subok ndmin]}]
  (if (ndarray? a)
    (if copy 
      (conversion/copy a :order order) 
      a)
    (asarray a)))

;; https://docs.scipy.org/doc/numpy/reference/generated/numpy.copy.html#numpy.copy
(defn copy
  "Return an array copy of the given object"
  [a & {:keys [order]
        :or {order \K}}]
  (conversion/copy (array a) :order order))


;;; Ones and zeros

;; https://docs.scipy.org/doc/numpy/reference/generated/numpy.empty.html#numpy.empty
(defn empty
  "Return a new array of given shape and type, without initializing entries"
  [shape & {:keys [dtype order]
            :or {dtype :dtype/float64 order \C}}]
  (ndarray shape dtype))

;; https://docs.scipy.org/doc/numpy/reference/generated/numpy.empty_like.html
(defn empty-like
  "Return a new array with the same shape and type as a given array"
  [a & {:keys [dtype order subok shape] :or {subok true}}]
  (let [_a (asarray a)]
    (empty (or shape (:size _a))
           :dtype (or dtype (:dtype _a))
           :order order)))

;; https://docs.scipy.org/doc/numpy/reference/generated/numpy.full.html
(defn full
  "Return a new array of given shape and type, filled with `fill_value`"
  [shape fill-value & {:keys [:dtype :order]
                       :or {dtype (dtype/infer-dtype [fill-value])
                            order \C}}]
  (let [a (empty shape
                 :dtype dtype
                 :order order)]
    (do
      (conversion/fill a fill-value)
      a)))

;; https://docs.scipy.org/doc/numpy/reference/generated/numpy.full_like.html
(defn full-like
  "Return a full array with the same shape and type as a given array"
  [a fill-value & {:keys [dtype order subok shape] :or {subok true}}]
  (let [_a (asarray a)]
    (full (or shape (:shape _a))
          fill-value
          :dtype (or dtype (:dtype _a))
          :order order)))

;; https://docs.scipy.org/doc/numpy/reference/generated/numpy.ones.html
(defn ones
  "Return a new array of given shape and type, filled with ones"
  [shape & {:keys [dtype order] :or {dtype :dtype/float64 order \C}}]
  (full shape
        1
        :dtype dtype
        :order order))

;; https://docs.scipy.org/doc/numpy/reference/generated/numpy.ones_like.html
(defn ones-like
  "Return an array of ones with the same shape and type as a given array"
  [a & {:keys [dtype order subok shape] :or {subok true}}]
  (let [_a (asarray a)]
    (full-like _a
               1
               :dtype dtype
               :order order
               :subok subok
               :shape shape)))

;; https://docs.scipy.org/doc/numpy/reference/generated/numpy.zeros.html
(defn zeros
  "Return a new array of given shape and type, filled with zeros"
  [shape & {:keys [dtype order] :or {dtype :dtype/float64 order \C}}]
  (full shape
        0
        :dtype dtype
        :order order))

;; https://docs.scipy.org/doc/numpy/reference/generated/numpy.zeros.html
(defn zeros-like
  "Return an array of zeros with the same shape and type as a given array"
  [a & {:keys [dtype order subok shape] :or {subok true}}]
  (let [_a (asarray a)]
    (full-like _a
               0
               :dtype dtype
               :order order
               :subok subok
               :shape shape)))

;; Creating record arrays (numpy.rec)
;; Creating character arrays (numpy.rec)
;; Numerical ranges
;; Building matrices
;; The Matrix class
