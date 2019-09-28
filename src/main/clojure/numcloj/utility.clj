(ns numcloj.utility)

(defn nan? {:tag Boolean} [x] (and (number? x) (Double/isNaN x)))
(defn not-nan? {:tag Boolean} [x] (not (nan? x)))
