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
        (is (= errors #{:existence-error}))))))

(deftest test-existence-valid
  (testing "Validating the existence of a given nonexistent cell"
    (let [input-types {"int_field" {"type" "Integer" "existence" true}}
          cell ["int_field" "1"]]
      (let [validated (validate-cell 1 input-types cell)
            errors (error-sieve validated)]
        (is (= errors #{}))))))

(deftest test-json-invalid
  (testing "Validating the json format of a cell with improperly formed JSON string content"
    (let [input-types {"json_field" {"type" "JSON"}}
          cell ["json_field" "invalid-json"]]
      (let [validated (validate-cell 1 input-types cell)
            errors (error-sieve validated)]
        (is (= errors #{:invalid-json-error}))))))

(deftest test-json-valid
  (testing "Validating the json format of a cell with improperly formed JSON string content"
    (let [input-types {"json_field" {"type" "JSON"}}
          cell ["json_field" "{}"]]
      (let [validated (validate-cell 1 input-types cell)
            errors (error-sieve validated)]
        (is (= errors #{}))))))

(deftest test-max-length-invalid
  (testing "Validating the length of a cell with a string that is too long"
    (let [input-types {"string_field" {"type" "String" "max" 5}}
          cell ["string_field" "hello world"]]
      (let [validated (validate-cell 1 input-types cell)
            errors (error-sieve validated)]
        (is (= errors #{:string-too-long-error}))))))

(deftest test-max-length-valid
  (testing "Validating the length of a cell with a string that is too long"
    (let [input-types {"string_field" {"type" "String" "max" 100}}
          cell ["string_field" "hello world"]]
      (let [validated (validate-cell 1 input-types cell)
            errors (error-sieve validated)]
        (is (= errors #{}))))))

(deftest test-email-format-invalid
  (testing "Validating the format of an invalid email"
    (let [input-types {"email_field" {"type" "Email"}}
          cell ["email_field" "not-an-email"]]
      (let [validated (validate-cell 1 input-types cell)
            errors (error-sieve validated)]
        (is (= errors #{:formatting-error}))))))


(deftest test-cell-validation
  (testing "Validating cells.  Requires csv-type-checker.impure/*raw-csv-data* to be populated."
    (let [raw-csv-data '(["int_field" "string_field"] ["1" "john"] ["2" "kate"] ["3(" "joe"] ["4" "mary"] ["1" "jake"])]
      (update-raw-csv-data! raw-csv-data)
      (test-validate-valid-cell)
      (test-validate-bad-cell)
      (test-uniqueness-invalid)
      (test-existence-invalid)
      (test-existence-valid)
      (test-json-invalid)
      (test-json-valid)
      (test-max-length-invalid)
      (test-max-length-valid)
      (test-email-format-invalid))))

(defn test-ns-hook [] 
  (test-transform-csv)
  (test-cell-validation))
