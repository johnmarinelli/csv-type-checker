(ns csv-type-checker.common
  (:require [clj-time.format :as time-format]))

(defn create-timestamp [input-fmt timestamp]
  (let [format (time-format/formatter input-fmt)]
    (time-format/parse format timestamp)))
