(ns bamboo.array-test
  (:refer-clojure :exclude [all any])
  (:require [clojure.test :refer [deftest is]]
            [numcloj.core :as np]
            [bamboo.array :refer [array to-numpy]]))

(deftest object-array-tests 
  (let [data [nil Double/NaN "a" 1 true 42.7]
        arr (array data)]
    (is (= {:objtype :objtype/extension-array 
            :dtype :dtype/object
            :shape [6 nil]}
           (select-keys arr [:objtype :dtype :shape])))
    (is (true? (np/array-equal data (to-numpy arr))))
    ; (is (= 42.7 (np/all (to-numpy arr))))
    ; (is (= "a" (np/any (to-numpy arr))))
    ; (is (thrown-with-msg? Exception #"^argmax not allowed for this dtype*" 
    ;                       (np/argmax (to-numpy arr))))
    ; (is (thrown-with-msg? Exception #"^argmin not allowed for this dtype*" 
    ;                       (np/argmin (to-numpy arr))))
    (is (thrown? ClassCastException (np/argsort (to-numpy arr))))
    (is (np/array-equal [1 0 2 3] (np/argsort [9 7.5 Double/NaN nil])))  
    ;(is (true? (np/any (np/isnan (to-numpy arr)))))
    ))
