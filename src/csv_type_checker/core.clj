(ns csv-type-checker.core
  (:require [clj-time.core :as time]
            [clj-time.format :as time-format]
            [clojure.string :refer [trim lower-case]]
            [csv-type-checker.errors.type-errors :refer :all]
            [csv-type-checker.errors.existence-errors :refer :all]
            [csv-type-checker.errors.uniqueness-errors :refer :all]
            [csv-type-checker.errors.all-errors :refer :all]
            [csv-type-checker.impure :refer :all])
  (:import [clojure.lang PersistentVector PersistentHashMap]))

(def resource-path (str (System/getProperty "user.dir") "/resources/"))

; impure
(defn validate-cell [^Long row-number ^PersistentHashMap input-types ^PersistentVector cell]
  (let [field-name (first cell)
        metadata (input-types field-name)
        field-type (metadata "type")
        raw-value (second cell)
        value (trim raw-value)
        all-constraints (concat existence-constraints uniqueness-constraints (type-constraints field-type))]
    (let [first-error (some error-filter (map #(% value metadata field-name) all-constraints)) ; returns the first error found.  this makes errors have priority
          msg (all-error-definitions first-error)
          validated (map #(% value metadata field-name) all-constraints)
          errors (error-sieve validated)
          select-values (comp vals select-keys)
          error-keys (into [] errors)
          msgs (select-values all-error-definitions error-keys)]
      (if (> (count errors) 0) ; if there are any type-error keys in possible-type-errors
        (println (str "Error at row " row-number ", field " field-name ": " msg))
        ;(println (str "No errors for row " row-number ", field " field-name))
        identity)
      validated)))
   
; `row` should be a transformed-csv row
(defn validate-row [^PersistentHashMap input-types ^PersistentVector row] 
  (swap! row-number inc) 
  (map (fn [cell] 
         (validate-cell @row-number input-types cell)) row))

; Go through each row and validate the value of each cell
(defn validate-csv [^PersistentHashMap input-types ^PersistentVector transformed-csv]
  (swap! row-number (fn [_] 0))
  (map (partial validate-row input-types) transformed-csv))

; pure

; transforms tabular data into associative array.  requires first row to be headers.
; [ { :field_1 => 'field_1_val', :field_2 => 'field_2_val'... } {} {}...]
(defn transform-csv [^PersistentVector csv]
  (let [headers (first csv)
        rows (rest csv)] 
    (map #(apply array-map (interleave headers %)) rows)))

; entry 
(defn -main [& args]
  (let [csv-filename (first args)
        types-filename (second args)]
    (-> csv-filename read-csv update-raw-csv-data! transform-csv (validate-csv (read-json types-filename)))))
