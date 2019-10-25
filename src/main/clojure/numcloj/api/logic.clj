(ns numcloj.api.logic
  (:require [numcloj.array-buffer :as b]
            [numcloj.array-creation :refer [asarray empty* frombuffer]]
            [numcloj.functional :refer [vectorize]]
            [numcloj.utility :refer [nan?]]))

;;;; Logic functions

(defn- element-wise-compare
  [f x1 x2 & {:keys [out where]}]
  (let [_x1 (asarray x1)
        _x2 (asarray x2)
        _out (or out (empty* (max (:size _x1) (:size _x2)) :dtype :dtype/bool))]
    (b/dual-map-values f _x1 _x2 _out)
    _out))

;;; Truth value testing (TODO: rewrite these using buffer!)

;; https://docs.scipy.org/doc/numpy/reference/generated/numpy.all.html
; (defmulti _all :dtype)
; (defmethod _all :default [a]
;   (throw (ex-info (str "cannot perform all with this array type: " (:dtype a)) {:type :TypeError})))
; (defmethod _all :dtype/bool [a] (every? true? ^booleans (:data a)))
; (defmethod _all :dtype/number [a] (every? (complement zero?) (:data a)))
; (defmethod _all :dtype/object [a]
;   ;; objects behave oddly...
;   (reduce #(if (and (some? %2) (not-nan? %2))
;              (condp instance? %2
;                Boolean (if (false? %2) (reduced %2) %2)
;                Number (if (zero? %2) (reduced %2) %2)
;                %2)
;              %1)
;           true (:data a)))
; (defn all
;   "Test whether all array elements along a given axis evaluate to True"
;   [a & {:keys [axis out keepdims]}]
;   (_all (asarray a)))

;; https://docs.scipy.org/doc/numpy/reference/generated/numpy.any.html
; (defmulti _any :dtype)
; (defmethod _any :default [a]
;   (throw (ex-info (str "cannot perform any with this array type: " (:dtype a))
;                   {:type :TypeError})))
; (defmethod _any :dtype/bool [a] (not-every? false? ^booleans (:data a)))
; (defmethod _any :dtype/number [a] (not-every? zero? (:data a)))
; (defmethod _any :dtype/object [a]
;   ;; objects behave oddly...
;   (reduce #(if (and (some? %2) (not-nan? %2))
;              (condp instance? %2
;                Boolean (if (true? %2) (reduced %2) %1)
;                Number (if-not (zero? %2) (reduced %2) %1)
;                (reduced %2))
;              %1)
;           false (:data a)))
; (defn any
;   "Test whether any array element along a given axis evaluates to True"
;   [a & {:keys [axis out keepdims]}]
;   (_any (asarray a)))

;;; Array contents

;; https://docs.scipy.org/doc/numpy/reference/generated/numpy.isnan.html#numpy.isnan
(defn isnan
  "Test element-wise for NaN and return result as a boolean array"
  [a & {:keys [out where]}]
  (let [vf (vectorize nan? :otypes [:dtype/bool])]
    (vf (asarray a))))

;;; Logical operations

;; https://docs.scipy.org/doc/numpy/reference/generated/numpy.logical_and.html
(defn logical-and
  "Compute the truth value of x1 AND x2 element-wise"
  [x1 x2 & {:keys [out where]}]
  (element-wise-compare #(and %1 %2) x1 x2 :out out :where where))

;; https://docs.scipy.org/doc/numpy/reference/generated/numpy.logical_or.html
(defn logical-or
  "Compute the truth value of x1 OR x2 element-wise"
  [x1 x2 & {:keys [out where]}]
  (element-wise-compare #(or %1 %2) x1 x2 
                        :out out :where where))

;; https://docs.scipy.org/doc/numpy/reference/generated/numpy.logical_not.html
(defn logical-not
  "Compute the truth value of NOT x element-wise"
  [x & {:keys [out where]}]
  (let [_x (asarray x)
        _out (or out (empty* :dtype/bool (:size _x)))]
    (b/map-values not _x _out)
    (frombuffer _out :dtype :dtype/bool)))

;;; Comparison

;; https://docs.scipy.org/doc/numpy/reference/generated/numpy.array_equal.html
(defn array-equal
  "True if two arrays have the same shape and elements, false otherwise"
  [a1 a2]
  (let [_a1 (asarray a1)
        _a2 (asarray a2)]
    (and (= (:dtype _a1) (:dtype _a2))
         (= (:shape _a1) (:shape _a2))
         (b/equals (:data _a1) (:data _a2)))))

;; https://docs.scipy.org/doc/numpy/reference/generated/numpy.equal.html
(defn equal
  "Return (x1 == x2) element-wise"
  [x1 x2 & {:keys [out where]}]
  (element-wise-compare = x1 x2 :out out :where where))

;; https://docs.scipy.org/doc/numpy/reference/generated/numpy.greater.html
(defn greater
  "Return (x1 > x2) element-wise"
  [x1 x2 & {:keys [out where]}]
  (element-wise-compare > x1 x2 :out out :where where))

;; https://docs.scipy.org/doc/numpy/reference/generated/numpy.greater_equal.html
(defn greater-equal
  "Return (x1 >= x2) element-wise"
  [x1 x2 & {:keys [out where]}]
  (element-wise-compare >= x1 x2 :out out :where where))

;; https://docs.scipy.org/doc/numpy/reference/generated/numpy.less.html
(defn less
  "Return (x1 < x2) element-wise"
  [x1 x2 & {:keys [out where]}]
  (element-wise-compare < x1 x2 :out out :where where))

;; https://docs.scipy.org/doc/numpy/reference/generated/numpy.less_equal.html
(defn less-equal
  "Return (x1 <= x2) element-wise"
  [x1 x2 & {:keys [out where]}]
  (element-wise-compare <= x1 x2 :out out :where where))

;; https://docs.scipy.org/doc/numpy/reference/generated/numpy.not_equal.html
(defn not-equal
  "Return (x1 != x2) element-wise"
  [x1 x2 & {:keys [out where]}]
  (element-wise-compare not= x1 x2 :out out :where where))
