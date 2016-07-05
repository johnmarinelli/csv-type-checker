(ns csv-type-checker.common-test
  (:require [clojure.test :refer :all] 
            [clj-time.core :as time]
            [csv-type-checker.common :refer :all]))

(deftest test-create-timestamp
  (testing "Creating a timestamp with a given format"
    (let [fmt "yyyy-MM-dd"
          ts "2016-05-05"
          r (create-timestamp fmt ts)]
      (is (time/equal? (time/date-time 2016 5 5) r)))))

