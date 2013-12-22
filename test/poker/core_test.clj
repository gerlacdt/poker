(ns poker.core-test
  (:require [clojure.test :refer :all]
            [poker.core :as p]
            [clojure.string :as string]))

(deftest a-test
  (testing "FIXED."
    (is (= 0 0))))


(deftest card-ranks-test
  (testing "calculate the correct card rank of a hand"
    (let [sf "6C 7C 8C 9C TC"
          fk "9D 9H 9S 9C 7D"
          fh "TD TC TH 7C 7D"]
      (is (= [9, 8, 7, 6, 5] (p/card-ranks "6C 7C 9C 8C 5C")))
      (is (= [14, 13, 12, 11 , 10] (p/card-ranks "TC JC QC KC AC")))
      (is (= [10, 9, 8, 7, 6] (p/card-ranks sf)))
      (is (= [9, 9, 9, 9, 7] (p/card-ranks fk)))
      (is (= [10, 10, 10, 7 , 7] (p/card-ranks fh)))
      (is (true? (p/flush sf)))
      (is (false? (p/flush fk)))
      (is (true? (p/straight [9, 8, 7, 6, 5])))
      (is (false? (p/straight [9, 8, 8, 6, 5])))
      (is (= 9 (p/kind p/card-ranks fk))))))

(deftest count-test
  (testing "count the frequency of a given element in a sequence"
    (is (= 4 (count-freq 9 [9 9 9 9 4])))))
