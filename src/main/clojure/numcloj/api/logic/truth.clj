(ns numcloj.api.logic.truth
  (:require [numcloj.array-creation :refer [asarray]]
            [numcloj.utility :refer [nan? not-nan?]]))

;;;; Logic functions

;;; Truth value testing

;; https://docs.scipy.org/doc/numpy/reference/generated/numpy.all.html
(defmulti _all :dtype)
(defmethod _all :default [a]
  (throw (ex-info (str "cannot perform all with this array type: " (:dtype a)) {:type :TypeError})))
(defmethod _all :dtype/bool [a] (every? true? ^booleans (:data a)))
(defmethod _all :dtype/number [a] (every? (complement zero?) (:data a)))
(defmethod _all :dtype/object [a]
  ;; objects behave oddly...
  (reduce #(if (and (some? %2) (not-nan? %2))
             (condp instance? %2
               Boolean (if (false? %2) (reduced %2) %2)
               Number (if (zero? %2) (reduced %2) %2)
               %2)
             %1)
          true (:data a)))
(defn all
  "Test whether all array elements along a given axis evaluate to True"
  [a & {:keys [axis out keepdims]}] 
  (_all (asarray a)))

;; https://docs.scipy.org/doc/numpy/reference/generated/numpy.any.html
(defmulti _any :dtype)
(defmethod _any :default [a]
  (throw (ex-info (str "cannot perform any with this array type: " (:dtype a)) 
                  {:type :TypeError})))
(defmethod _any :dtype/bool [a] (not-every? false? ^booleans (:data a)))
(defmethod _any :dtype/number [a] (not-every? zero? (:data a)))
(defmethod _any :dtype/object [a]
  ;; objects behave oddly...
  (reduce #(if (and (some? %2) (not-nan? %2))
             (condp instance? %2
               Boolean (if (true? %2) (reduced %2) %1)
               Number (if-not (zero? %2) (reduced %2) %1)
               (reduced %2))
             %1)
          false (:data a)))
(defn any
  "Test whether any array element along a given axis evaluates to True"
  [a & {:keys [axis out keepdims]}] 
  (_any (asarray a)))