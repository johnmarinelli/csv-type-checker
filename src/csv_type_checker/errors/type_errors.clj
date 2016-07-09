(ns csv-type-checker.errors.type-errors
  (:require [csv-type-checker.common :as common]
            [clojure.data.json :as json]))

(def type-error-definitions {:formatting-error "Format error." 
                             :invalid-json-error "Invalid JSON error."
                             :string-too-long-error "String is too long."})
(def type-errors (keys type-error-definitions))

; type => { constraints }
(def type-constraints { "String" [(fn [str metadata _]
                                    (let [check-for-length? (contains? metadata "max")
                                          a (println (count str))
                                          b (println (Integer. (metadata "max")))]
                                      (if check-for-length?
                                        (if (> (count str) (Integer. (metadata "max")))
                                          :string-too-long-error
                                          str)
                                        str)))]
                        "Integer" [(fn [i _ _] (when (not (= i "")) (try (Integer. i) (catch Exception e :formatting-error))))]
                        "Float" [(fn [f _ _] (when (not (= f "")) (try (Double. f) (catch Exception e :formatting-error))))]
                        "Timestamp" [(fn [timestamp metadata _]
                                       (when (not (= timestamp "")) (try (common/create-timestamp (metadata "format") timestamp)
                                                     (catch Exception e :formatting-error))))]
                        "JSON" [(fn [json-str metadata _]
                                  (when (not (= json-str "")) 
                                    (try (json/read-str json-str)
                                         (catch Exception e :invalid-json-error))))]})
