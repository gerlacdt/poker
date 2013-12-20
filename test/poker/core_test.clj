(ns poker.core-test
  (:require [clojure.test :refer :all]
            [poker.core :refer :all]))

(deftest a-test
  (testing "FIXED."
    (is (= 0 0))))


(deftest card-ranks-test
  (testing "calculate the correct card rank of a hand"
    (is (= [9, 8, 7, 6, 5] (card-ranks "6C 7C 9C 8C 5C")))
    (is (= [14, 13, 12, 11 , 10] (card-ranks "TC JC QC KC AC")))))
