(ns numcloj.array-creation
  (:refer-clojure :exclude [empty])
  (:require [numcloj.array-buffer :as b]
            [numcloj.array.conversion :as conversion]
            [numcloj.dtype :as dtype]))

;;;; Array creation routines
;;;; https://docs.scipy.org/doc/numpy/reference/routines.array-creation.html

;;; helpers
(defmulti shaper class)
(defmethod shaper :default [shape] shape)
(defmethod shaper Number [shape] [(long shape) nil])

;;; ndarray
(defn- _size [data] (b/size data))
(defn- _itemsize [data] (* 8 (_size data)))
(defn- _ndim [data] 1)
(defn- _nbytes [data] (* (_ndim data) (_itemsize data)))
(defn- _strides [data] [(_itemsize data) nil])

;;; ndarray
(defn- ndarray
  [shape dtype & {:keys [buffer offset strides order]}]
  (let [_shape (shaper shape)
        data (or buffer (b/array dtype (first _shape)))]
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

(defn ndarray-expr? [a]
  (and (map? a) (ndarray? (:array a)) ; (ifn? (:expr a))
       ))

;;; From existing data

;; https://docs.scipy.org/doc/numpy/reference/generated/numpy.asarray.html#numpy.asarray
(defn- asclojurearray [a]
  (let [dtype (dtype/infer-dtype a)]
    (ndarray (count a) dtype :buffer (b/from-sequential dtype a))))
(defmulti asarray
  "Convert the input to an array"
  class)
(defmethod asarray :default [a] 
  (cond
    (ndarray-expr? a)
    (let [size (:size (:array a))
          dtype (:dtype a)]
      (ndarray (shaper size)
               dtype
               :buffer (b/map-values (:expr a)
                                     (:array a)
                                     (asarray (b/array dtype size)))))
    (ndarray? a) a
    :else nil))

(defmethod asarray (Class/forName "[Z") [a] (ndarray (alength a) :dtype/bool :buffer a))
(defmethod asarray (Class/forName "[D") [a] (ndarray (alength a) :dtype/float64 :buffer a))
(defmethod asarray (Class/forName "[J") [a] (ndarray (alength a) :dtype/int64 :buffer a))
(defmethod asarray (Class/forName "[Ljava.lang.Object;") [a] (ndarray (alength a) :dtype/object :buffer a))
(defmethod asarray clojure.lang.PersistentVector [a] (asclojurearray a))
(defmethod asarray clojure.lang.PersistentList [a] (asclojurearray a))
(defmethod asarray clojure.lang.PersistentHashSet [a] (asclojurearray a))
(defmethod asarray clojure.lang.ArraySeq [a] (asclojurearray (vec a)))
(defmethod asarray clojure.lang.LazySeq [a] (asclojurearray (vec a)))
(defmethod asarray clojure.lang.LongRange [a] (asclojurearray (vec a)))
(defmethod asarray clojure.lang.Repeat [a] (asclojurearray (vec a)))
(defmethod asarray java.lang.Boolean [a] (asclojurearray (vector a)))
(defmethod asarray java.lang.Double [a] (asclojurearray (vector a)))
(defmethod asarray java.lang.Long [a] (asclojurearray (vector a)))
(defmethod asarray java.lang.String [a] (asclojurearray (vector a)))

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
  (conversion/copy (asarray a) :order order))


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
        (if (= dtype :dtype/bool) true 1)
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
        (if (= dtype :dtype/bool) false 0)
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
