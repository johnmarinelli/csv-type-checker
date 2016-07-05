(ns csv-type-checker.errors.all-errors
  (:require [csv-type-checker.errors.type-errors :refer :all]
            [csv-type-checker.errors.uniqueness-errors :refer :all]
            [csv-type-checker.errors.existence-errors :refer :all]))

(def all-error-definitions (merge type-error-definitions 
                                  uniqueness-error-definitions 
                                  existence-error-definitions))

(def error-filter (clojure.set/union (set type-errors) 
                                     (set uniqueness-errors) 
                                     (set existence-errors)))

(defn error-sieve [validated] 
  (clojure.set/intersection (set validated) error-filter))




