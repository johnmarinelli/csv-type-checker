(ns csv-type-checker.core
  (:require [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [clojure.data.json :as json])
  (:import [clojure.lang PersistentVector PersistentHashMap]))

; impure
(defn read-csv [^String filename]
  (with-open [f (io/reader filename)]
    (doall (csv/read-csv f))))

(defn read-json [^String filename]
  (let [contents (slurp filename)]
    (json/read-str contents)))

; pure
(defn try-catch [f & args] (try (apply f args) (catch Exception e (.getMessage e))))

; type => { constraints }
(def types {"String" []
            "Integer" [(fn [i] (try (Integer. i) (catch Exception e nil)))]})

(defmulti parse-field (fn [t v] (keyword t)))
(defmethod parse-field :String [t v] v)
(defmethod parse-field :Integer [t v] (Integer. v))

; [ { :field_1 => 'field_1_val', :field_2 => 'field_2_val'... } {} {}...]
(defn transform-csv [^PersistentVector csv]
  (let [headers (first csv)
        rows (rest csv)] 
    (map (fn [row] (apply array-map (interleave headers row))) rows)))

(defn validate-cell [^Long row-number ^PersistentHashMap input-types cell]
  (let [field-name (first cell)
        t (input-types field-name)
        value (second cell)
        constraints (types t)
        validated (map #(% value) constraints)]
    (if (some nil? validated)
      (println (str "Error found at row " row-number ".  Column: " field-name))
      (parse-field t value))))
   
(def row-number (atom 1)) 
; `row` should be a transformed-csv row
(defn validate-row [^PersistentHashMap input-types ^PersistentVector row] 
  (map (fn [cell] (swap! row-number inc) (validate-cell @row-number input-types cell)) row))

; Go through each row and validate the value of each cell
(defn validate-csv [^PersistentHashMap input-types ^PersistentVector transformed-csv]
  (map (partial validate-row input-types) transformed-csv))

(defn -main [& args]
  (let [csv-filename (first args)
        types-filename (second args)]
    (-> csv-filename read-csv transform-csv (validate-csv (read-json types-filename)))))
