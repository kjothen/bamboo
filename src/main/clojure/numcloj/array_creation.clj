(ns numcloj.array-creation
  (:require [numcloj.array-buffer :as b]
            [numcloj.array.conversion :as conversion]
            [numcloj.dtype :as dtype]))

;;;; Array creation routines
;;;; https://docs.scipy.org/doc/numpy/reference/routines.array-creation.html

;;; ndarray
(defn- ndarray
  [shape dtype & {:keys [buffer offset strides order]}]
  (let [len (if (sequential? shape) (first shape) shape)
        data (or buffer (b/array dtype len))]
    {:data data
     :dtype dtype
     :size (b/size data)
     :shape [len nil]}))

(defn ndarray? [a]
  (and (map? a) (isa? dtype/numcloj-hierarchy (:dtype a) :dtype/numcloj)))

(defn ndarray-expr? [a]
  (and (map? a) (ndarray? (:array a)) (ifn? (:expr a))))

;;; Forward declarations
(declare copy)

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
    (let [len (:size (:array a))
          dtype (:dtype a)]
      (ndarray len
               dtype
               :buffer (b/map-values (:expr a)
                                     (:array a)
                                     (asarray (b/array dtype len)))))
    (ndarray? a) a
    (sequential? a) (asclojurearray (vec a))
    :else (throw (ex-info (str "Cannot create ndarray from: " (type a) " " a)
                          {:type :ValueError}))))

(defmethod asarray (Class/forName "[Z") [a] 
  (ndarray (alength a) :dtype/bool :buffer a))
(defmethod asarray (Class/forName "[D") [a] 
  (ndarray (alength a) :dtype/float64 :buffer a))
(defmethod asarray (Class/forName "[J") [a] 
  (ndarray (alength a) :dtype/int64 :buffer a))
(defmethod asarray (Class/forName "[Ljava.lang.Object;") [a] 
  (ndarray (alength a) :dtype/object :buffer a))
(defmethod asarray java.lang.Boolean [a] (asclojurearray (vector a)))
(defmethod asarray java.lang.Double [a] (asclojurearray (vector a)))
(defmethod asarray java.lang.Long [a] (asclojurearray (vector a)))
(defmethod asarray java.lang.String [a] (asclojurearray (vector a)))

;; https://docs.scipy.org/doc/numpy/reference/generated/numpy.array.html#numpy.array
(defn array
  "Create an array"
  [a & {:keys [dtype copy order subok ndmin]}]
  (cond
    (ndarray? a) (if copy (conversion/copy a :order order) a)
    :else (asarray a)))

;; https://docs.scipy.org/doc/numpy/reference/generated/numpy.copy.html#numpy.copy
(defn copy
  "Return an array copy of the given object"
  [a & {:keys [order]
        :or {order \K}}]
  (conversion/copy (asarray a) :order order))

;; https://docs.scipy.org/doc/numpy/reference/generated/numpy.frombuffer.html
(defn frombuffer
  "Interpret a buffer as a 1-dimensional array"
  [buffer & {:keys [dtype count* offset] 
             :or {dtype :dtype/float64 count* -1 offset 0}}]
  (if (= -1 count*)
    (ndarray (b/size buffer) dtype :buffer buffer :offset offset)
    (ndarray count* dtype :buffer buffer :offset offset)))

;;; Ones and zeros

;; https://docs.scipy.org/doc/numpy/reference/generated/numpy.empty.html#numpy.empty
(defn empty*
  "Return a new array of given shape and type, without initializing entries"
  [shape & {:keys [dtype order]
            :or {dtype :dtype/float64 order \C}}]
  (ndarray shape dtype))

;; https://docs.scipy.org/doc/numpy/reference/generated/numpy.empty_like.html
(defn empty-like
  "Return a new array with the same shape and type as a given array"
  [a & {:keys [dtype order subok shape] :or {subok true}}]
  (let [_a (asarray a)]
    (empty* (or shape (:size _a))
           :dtype (or dtype (:dtype _a))
           :order order)))

;; https://docs.scipy.org/doc/numpy/reference/generated/numpy.full.html
(defn full
  "Return a new array of given shape and type, filled with `fill_value`"
  [shape fill-value & {:keys [:dtype :order]
                       :or {dtype (dtype/infer-dtype [fill-value])
                            order \C}}]
  (let [a (empty* shape
                 :dtype dtype
                 :order order)]
    (conversion/fill a fill-value)
    a))

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

;;; Creating record arrays (numpy.rec)

;; https://docs.scipy.org/doc/numpy/reference/generated/numpy.core.records.array.html
(defn recarray
  "Construct an ndarray that allows field access using attributes"
  [shape & {:keys [dtype formats names buf]}]
  (assoc (ndarray shape :dtype/record)
         :names names))

;;; Creating character arrays (numpy.rec)
;;; Numerical ranges

;; https://docs.scipy.org/doc/numpy/reference/generated/numpy.arange.html
(defn arange
  "Return evenly spaced values within a given interval"
  ([stop] (arange 0 stop 1))
  ([start stop] (arange start stop 1))
  ([start stop step]
   (let [float64? (or (double? start) (double? stop) (double? step))
         _start (if float64? (double start) (long start))
         _stop (if float64? (double stop) (long stop))
         _step (if float64? (double step) (long step))
         dtype (if float64? :dtype/float64 :dtype/int64)]
     (frombuffer (b/arange _start _stop _step dtype)
                 :dtype dtype))))

;;; Building matrices
;;; The Matrix class
