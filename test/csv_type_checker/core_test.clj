(ns csv-type-checker.core-test
  (:require [clojure.test :refer :all]
            [csv-type-checker.impure :refer :all]
            [csv-type-checker.errors.all-errors :refer :all]
            [csv-type-checker.core :refer :all]))

(deftest test-transform-csv
  (testing "Transforming valid tabular data into associative array"
    (let [csv [["int_field" "string_field"] ["1" "john"] ["2" "kate"] ["3(" "joe"] ["4" "mary"]]
          tcsv (transform-csv csv)]
      (is (= tcsv '({"int_field" "1", "string_field" "john"} {"int_field" "2", "string_field" "kate"} {"int_field" "3(", "string_field" "joe"} {"int_field" "4", "string_field" "mary"}))))))

(deftest test-validate-valid-cell
  (testing "Validating a valid cell against constraints depending on its value's type"
    (let [input-types {"int_field" {"type" "Integer"}}
          cell ["int_field" "2"]]
      (let [validated (validate-cell 1 input-types cell)
            errors (error-sieve validated)]
        (is (= errors #{}))))))

(deftest test-validate-bad-cell
  (testing "Validating an invalid cell against constraints depending on its value's type"
    (let [input-types {"int_field" {"type" "Integer"}}
          cell ["int_field" "1.0"]
          validated (validate-cell 1 input-types cell)
          errors (error-sieve validated)]
      (is (= errors #{:formatting-error})))))

(deftest test-uniqueness-invalid
  (testing "Validating the uniqueness of a given non-unique cell"
    (let [input-types {"int_field" {"type" "Integer", "uniqueness" true}}
          cell ["int_field" "1"]
          validated (validate-cell 1 input-types cell)
          errors (error-sieve validated)]
      (is (= errors #{:uniqueness-error})))))

(deftest test-existence-invalid
  (testing "Validating the existence of a given nonexistent cell"
    (let [input-types {"int_field" {"type" "Integer" "uniqueness" true "existence" true}}
          cell ["int_field" ""]]
      (let [validated (validate-cell 1 input-types cell)
            errors (error-sieve validated)]
        (is (= errors #{:existence-error :formatting-error}))))))

(deftest test-cell-validation
  (testing "Validating cells.  Requires csv-type-checker.impure/*raw-csv-data* to be populated."
    (let [raw-csv-data '(["int_field" "string_field"] ["1" "john"] ["2" "kate"] ["3(" "joe"] ["4" "mary"] ["1" "jake"])]
      (update-raw-csv-data! raw-csv-data)
      (test-validate-valid-cell)
      (test-validate-bad-cell)
      (test-uniqueness-invalid)
      (test-existence-invalid))))

(defn test-ns-hook [] 
  (test-transform-csv)
  (test-cell-validation))
