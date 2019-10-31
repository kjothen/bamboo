(ns examples
  (:require [io.aviso.ansi :as ansi]
            [bamboo.checked.core :as pd]
            [bamboo.checked.dataframe :as dataframe]
            [bamboo.lang :refer [slice]]))

(defn fancy-print [s] (println) (println (ansi/green s)) (println))

(defn -main []
  (let [_ (fancy-print "Create a dataframe from a CSV file:")
        df (pd/read-csv "kepler.csv.gz" :skiprows 53)
        cols ["kepid" "kepoi_name" "kepler_name" "koi_disposition" "koi_score"]]
    (fancy-print "Show a snippet of the dataframe:")
    (pd/show df :max-cols 6 :max-rows 5 :show-dimensions true)

    (fancy-print "Show all the columns:")
    (pd/show (:columns df))

    (fancy-print "Show data for specific columns:")
    (pd/show df :columns cols :max-rows 4)

    (let [_ (fancy-print (str "Select confirmed exoplanets with "
                              "a disposition score equal to 1.0:"))
          dfx (partial dataframe/expr df)
          cond1 (pd/equal (dfx "koi_disposition") "CONFIRMED")
          cond2 (pd/equal (dfx "koi_score") 1.0)]
      (pd/show (dfx (pd/logical-and cond1 cond2)) :columns cols :max-rows 4))

    (fancy-print "Show columns upto and include 'koi_score':")
    (pd/show (dataframe/loc df (slice) (slice :end "koi_score")) :max-rows 4)

    (let [_ (fancy-print "Take rows and columns of interest:")
          dfx (partial dataframe/expr df)
          cond1 (pd/equal (dfx "koi_disposition") "CONFIRMED")
          cond2 (pd/equal (dfx "koi_score") 1.0)
          df-interest (dataframe/loc df
                                     (pd/logical-and cond1 cond2)
                                     (slice :end "koi_score"))]
      (pd/show df-interest :max-rows 4))

    (let [_ (fancy-print
             (str "Create a dataframe from collection data, named columns "
                  "and periodic datetimes for the index:"))
          dates (pd/date-range :start "2019-01-01" :periods 5 :freq "min")
          data (partition 4 (range 20))
          df-data (pd/dataframe data :columns ["w" "x" "y" "z"] :index dates)]
      (pd/show df-data)

      (fancy-print "Show the datetime index:")
      (pd/show (:index df-data))))
 (shutdown-agents))
