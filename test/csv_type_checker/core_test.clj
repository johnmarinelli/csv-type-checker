(ns csv-type-checker.core-test
  (:require [clojure.test :refer :all]
            [csv-type-checker.core :refer :all]
            [clj-time.core :as time]))

(deftest test-create-timestamp
  (testing "Creating a timestamp with a given format"
    (let [fmt "yyyy-MM-dd"
          ts "2016-05-05"
          r (create-timestamp fmt ts)]
      (is (time/equal? (time/date-time 2016 5 5) r)))))

(deftest test-transform-csv
  (testing "Transforming valid tabular data into associative array"
    (let [csv [["int_field" "string_field"] ["1" "john"] ["2" "kate"] ["3(" "joe"] ["4" "mary"]]
          tcsv (transform-csv csv)]
      (is (= tcsv '({"int_field" "1", "string_field" "john"} {"int_field" "2", "string_field" "kate"} {"int_field" "3(", "string_field" "joe"} {"int_field" "4", "string_field" "mary"}))))))

(deftest test-validate-cell
  (testing "Validating a valid cell against constraints depending on its value's type"
    (let [input-types {"int_field" {"type" "Integer"}, "string_field" {"type" "String"}}
          cell ["int_field" "1"]
          validated (validate-cell 1 input-types cell)]
      (is (= validated '(1))))))

(deftest test-validate-bad-cell
  (testing "Validating an invalid cell against constraints depending on its value's type"
    (let [input-types {"int_field" {"type" "Integer"}, "string_field" {"type" "String"}}
          cell ["int_field" "1.0"]
          validated (validate-cell 1 input-types cell)]
      (is (= validated '(:formatting-error))))))
