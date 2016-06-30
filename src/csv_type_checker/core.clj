(ns csv-type-checker.core
  (:require [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [clojure.data.json :as json]
            [clj-time.core :as time]
            [clj-time.format :as time-format]
            [clojure.string :refer [trim lower-case]])
  (:import [clojure.lang PersistentVector PersistentHashMap]))

(def resource-path (str (System/getProperty "user.dir") "/resources/"))

; impure
(defn read-csv [^String filename]
  (with-open [f (io/reader filename)]
    (doall (csv/read-csv f))))

(defn read-json [^String filename]
  (let [contents (slurp filename)]
    (json/read-str contents)))

(def row-number (atom 0)) 
; pure
(comment(defn try-catch [f & args] (try (apply f args) (catch Exception e (.getMessage e)))))

(defn create-timestamp [input-fmt timestamp]
  (let [format (time-format/formatter input-fmt)]
    (time-format/parse format timestamp)))

(def type-error-definitions {:formatting-error "Format error."})
(def type-errors (keys type-error-definitions))


; type => { constraints }
(def type-constraints { "String" []
                        "Integer" [(fn [i _] (try (Integer. i) (catch Exception e :formatting-error)))]
                        "Float" [(fn [f _] (try (Double. f) (catch Exception e :formatting-error)))]
                        "Timestamp" [(fn [timestamp metadata]
                                       (try (create-timestamp (metadata "format") timestamp)
                                            (catch Exception e :formatting-error)))]})

;(defmulti parse-typed-value (fn [t _] (lower-case (t "type"))))
;(defmethod parse-typed-value "string" [_ v] v)
;(defmethod parse-typed-value "integer" [_ v] (Integer. v))
;(defmethod parse-typed-value "float" [_ v] (Float. v))
;(defmethod parse-typed-value "timestamp" [t v]
;  (let [input-fmt (t "format")]
;    (create-timestamp input-fmt v)))
;

; transforms tabular data into associative array.  requires first row to be headers.
; [ { :field_1 => 'field_1_val', :field_2 => 'field_2_val'... } {} {}...]
(defn transform-csv [^PersistentVector csv]
  (let [headers (first csv)
        rows (rest csv)] 
    (map #(apply array-map (interleave headers %)) rows)))

(defn validate-cell [^Long row-number ^PersistentHashMap input-types cell]
  (let [field-name (first cell)
        metadata (input-types field-name)
        field-type (metadata "type")
        raw-value (second cell)
        value (trim raw-value)
        constraints (type-constraints field-type)]
    (when (not (= "" value))
      (let [validated (map #(% value metadata) constraints)
            errors (clojure.set/intersection (set validated) (set type-errors))]
        (if (> (count errors) 0) ; if there are any type-error keys in possible-type-errors
          (println (str "Error at row " row-number ", field " field-name))
          (println (str "No errors for row " row-number ", field " field-name)))
        validated))))
   
; `row` should be a transformed-csv row
(defn validate-row [^PersistentHashMap input-types ^PersistentVector row] 
  (swap! row-number inc) 
  (map (fn [cell] 
         (validate-cell @row-number input-types cell)) row))

; Go through each row and validate the value of each cell
(defn validate-csv [^PersistentHashMap input-types ^PersistentVector transformed-csv]
  (swap! row-number (fn [_] 0))
  (map (partial validate-row input-types) transformed-csv))

(defn -main [& args]
  (let [csv-filename (first args)
        types-filename (second args)]
    (-> csv-filename read-csv transform-csv (validate-csv (read-json types-filename)))))
