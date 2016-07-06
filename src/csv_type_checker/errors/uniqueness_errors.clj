(ns csv-type-checker.errors.uniqueness-errors
  (:require [csv-type-checker.impure :refer :all]))

(def uniqueness-error-definitions {:uniqueness-error "Uniqueness error."})
(def uniqueness-errors (keys uniqueness-error-definitions))

(def uniqueness-constraints [(fn [value metadata field-name]
                               (let [check-for-uniqueness? (boolean (Boolean/valueOf (metadata "uniqueness")))] 
                                 (if check-for-uniqueness?
                                   (let [headers (first (deref *raw-csv-data*))
                                         column-index (.indexOf headers field-name)
                                         colvals (get-column column-index)
                                         v (filter #(= % value) colvals)
                                         v-count (count v)
                                         unique? (<= v-count 1)]
                                     (if unique? value :uniqueness-error))
                                   value)))])
