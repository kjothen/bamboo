(ns bamboo.series-test
  (:require [clojure.test :refer [deftest is]]
            [io.aviso.ansi :as ansi]
            [bamboo.series :refer [iat iat! series show]]))

(def tuple [99.0 -42.0 0.0 Double/NaN "abc"])
(def obj-index ["m" "n" "o" "p" "q"])

(defn print-series [s] (show s) (println))

(deftest iat-tests
  (let [index obj-index
        s (atom (series tuple :index index))]
    (println (ansi/green (format "(def s (series %s :index %s))"
                                 tuple index)))
    (print-series @s)
    (is (= -42.0 (iat @s 1)))
    (iat! s 1 "xyz")
    (println (ansi/green (format "(iat! s 1 \"xyz\")" tuple index)))
    (print-series @s)
    (is (= "xyz" (iat @s 1)))))
