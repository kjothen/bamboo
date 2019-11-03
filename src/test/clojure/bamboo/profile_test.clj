(ns bamboo.profile-test
  (:require [bamboo.core :as pd]
            [taoensso.tufte :as tufte]))

(tufte/add-basic-println-handler!
 {:format-pstats-opts {:columns [:n-calls :p50 :mean :clock :total]
                       :format-id-fn name}})

(defn profile
  ([n] (profile n nil))
  ([n nrows]
   (tufte/profile
    {}
    (dotimes [_ n]
      (pd/read-csv "kepler.csv.gz" :nrows nrows)))))
