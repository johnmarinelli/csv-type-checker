(ns csv-type-checker.core
  (:require [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [clojure.data.json :as json]
            [clj-time.core :as time]
            [clj-time.format :as time-format]
            [clojure.string :refer [trim lower-case]])
  (:import [clojure.lang PersistentVector PersistentHashMap]))

(declare uniqueness-constraints existence-constraints type-constraints all-error-definitions error-sieve)

(def resource-path (str (System/getProperty "user.dir") "/resources/"))

; impure
(def ^:dynamic *raw-csv-data* (atom nil))
(defn update-raw-csv-data! [d]
  (reset! *raw-csv-data* d))

(defn get-column [^Long col-idx]
  (let [raw-csv-data (deref *raw-csv-data*)
        rows (rest raw-csv-data)]
    (map #(nth % col-idx) rows)))

(defn read-csv [^String filename]
  (with-open [f (io/reader filename)]
    (doall (csv/read-csv f))))

(defn read-json [^String filename]
  (let [contents (slurp filename)]
    (json/read-str contents)))

(def row-number (atom 0)) 

(defn validate-cell [^Long row-number ^PersistentHashMap input-types ^PersistentVector cell]
  (let [field-name (first cell)
        metadata (input-types field-name)
        field-type (metadata "type")
        raw-value (second cell)
        value (trim raw-value)
        all-constraints (concat uniqueness-constraints existence-constraints (type-constraints field-type))]
    (let [validated (map #(% value metadata field-name) all-constraints)
          errors (error-sieve validated)
          select-values (comp vals select-keys)
          error-keys (into [] errors)
          msgs (select-values all-error-definitions error-keys)]
      (if (> (count errors) 0) ; if there are any type-error keys in possible-type-errors
        (println (str "Error at row " row-number ", field " field-name ": " msgs))
        (println (str "No errors for row " row-number ", field " field-name)))
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

(def uniqueness-error-definitions {:uniqueness-error "Uniqueness error."})
(def uniqueness-errors (keys uniqueness-error-definitions))

(def uniqueness-constraints [(fn [value metadata field-name]
                               (let [headers (first (deref *raw-csv-data*))
                                     column-index (.indexOf headers field-name)
                                     colvals (get-column column-index)
                                     v (filter #(= % value) colvals)
                                     v-count (count v)
                                     unique? (<= v-count 1)]
                                 (if unique? value :uniqueness-error)))])

; pure
(defn create-timestamp [input-fmt timestamp]
  (let [format (time-format/formatter input-fmt)]
    (time-format/parse format timestamp)))

(def type-error-definitions {:formatting-error "Format error."})
(def type-errors (keys type-error-definitions))

; type => { constraints }
(def type-constraints { "String" []
                        "Integer" [(fn [i _ _] (try (Integer. i) (catch Exception e :formatting-error)))]
                        "Float" [(fn [f _ _] (try (Double. f) (catch Exception e :formatting-error)))]
                        "Timestamp" [(fn [timestamp metadata _]
                                       (try (create-timestamp (metadata "format") timestamp)
                                            (catch Exception e :formatting-error)))]})

(def existence-error-definitions {:existence-error "Existence error."})
(def existence-errors (keys existence-error-definitions))

(def existence-constraints [(fn [value metadata field-name]
                              (let [check-for-existence? (boolean (Boolean/valueOf (metadata "existence")))]
                                (if (and check-for-existence? (= value "")) 
                                  :existence-error
                                  value)))])

(def all-error-definitions (merge type-error-definitions uniqueness-error-definitions existence-error-definitions))
(def error-filter (clojure.set/union (set type-errors) (set uniqueness-errors) (set existence-errors)))
(defn error-sieve [validated] 
  (clojure.set/intersection (set validated) error-filter))

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
    (-> csv-filename read-csv transform-csv (validate-csv (read-json types-filename)))))
