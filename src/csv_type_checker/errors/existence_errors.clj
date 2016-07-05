(ns csv-type-checker.errors.existence-errors)

(def existence-error-definitions {:existence-error "Existence error."})
(def existence-errors (keys existence-error-definitions))

(def existence-constraints [(fn [value metadata field-name]
                              (let [check-for-existence? (boolean (Boolean/valueOf (metadata "existence")))]
                                (if (and check-for-existence? (= value "")) 
                                  :existence-error
                                  value)))])
