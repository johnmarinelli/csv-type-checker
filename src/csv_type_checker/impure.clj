(ns csv-type-checker.impure
  (:require [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [clojure.data.json :as json]))

(def ^:dynamic *raw-csv-data* (atom nil))
(defn update-raw-csv-data! [d]
  (reset! *raw-csv-data* d)
  d)

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

