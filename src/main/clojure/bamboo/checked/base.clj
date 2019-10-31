(ns bamboo.checked.base
  (:require [clojure.spec.alpha :as s]))

(s/def ::scalar? (s/nilable (s/or :b boolean? :i int? :d double? :s string?)))

(s/def ::array-like? (s/coll-of ::scalar?))
(s/def ::datetime-like? string?)

