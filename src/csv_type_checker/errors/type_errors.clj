(ns csv-type-checker.errors.type-errors
  (:require [csv-type-checker.common :as common]))

(def type-error-definitions {:formatting-error "Format error."})
(def type-errors (keys type-error-definitions))

; type => { constraints }
(def type-constraints { "String" []
                        "Integer" [(fn [i _ _] (try (Integer. i) (catch Exception e :formatting-error)))]
                        "Float" [(fn [f _ _] (try (Double. f) (catch Exception e :formatting-error)))]
                        "Timestamp" [(fn [timestamp metadata _]
                                       (try (common/create-timestamp (metadata "format") timestamp)
                                            (catch Exception e :formatting-error)))]})