(ns numcloj.test-utility
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as sgen]
            [numcloj.array-buffer :refer [from-sequential]]
            [numcloj.array-creation :refer [asarray ones zeros]]
            [numcloj.array.item-manipulation :refer [put]]))

;; fixtures
(def num-samples 10)
(def sample-range (range 0 num-samples))

(def vs {:dtype/bool (sgen/sample (s/gen boolean?) num-samples)
         :dtype/float64 (sgen/sample (s/gen double?) num-samples)
         :dtype/int64 (sgen/sample (s/gen int?) num-samples)
         :dtype/object (sgen/sample (s/gen string?) num-samples)})

(defn random-values []
  [(sgen/generate (s/gen boolean?))
   (sgen/generate (s/gen double?))
   (sgen/generate (s/gen int?))
   (sgen/generate (s/gen string?))])

(def as (atom nil))

(defn buffer-fixture [f]
  (reset! as (mapv (fn [[k v]] (from-sequential k v)) vs))
  (f)
  (reset! as nil))

(defn array-fixture [f]
  (reset! as (mapv (fn [[k v]] (asarray v)) vs))
  (f)
  (reset! as nil))

(defn random-sample [] (max 1 (rand-int num-samples)))

(defn random-samples []
  (distinct (take (random-sample) (repeatedly random-sample))))

(defn invert-samples [samples]
  (keep #(if-not (some #{%} samples) %) sample-range))

(defn samples->mask [samples]
  (put (zeros num-samples :dtype :dtype/int64)
       samples
       (ones (count samples) :dtype :dtype/int64)))

(defn samples->inverse-mask [samples]
  (put (ones num-samples :dtype :dtype/int64)
       samples
       (zeros (count samples) :dtype :dtype/int64)))