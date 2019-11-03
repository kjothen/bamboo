(ns bamboo.array-test
  (:refer-clojure :exclude [all any])
  (:require [clojure.test :refer [deftest is]]
            [io.aviso.ansi :as ansi]
            [numcloj.core :as np]
            [bamboo.array :refer [array show to-numpy]]))

(defn print-array [a] (show a :max-rows 100) (println))

(deftest object-array-tests 
  (let [data [nil Double/NaN "a" 1 true 42.7]
        arr (array data)]
    (println (ansi/green (format "(array %s)" data)))
    (print-array arr)
    (is (= {:objtype :objtype/extension-array 
            :dtype :dtype/object
            :shape [6 nil]}
           (select-keys arr [:objtype :dtype :shape])))
    (is (true? (np/array-equal data (to-numpy arr))))
    (is (thrown? ClassCastException (np/argsort (to-numpy arr))))
    (is (np/array-equal [1 0 2 3] (np/argsort [9 7.5 Double/NaN nil])))  
    ))
