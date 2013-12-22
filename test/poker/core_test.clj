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
          fh "TD TC TH 7C 7D"
          tp "5S 5D 9H 9C 6S"
          s1 "AS 2S 3S 4S 5S"]
      (is (= [9, 8, 7, 6, 5] (p/card-ranks "6C 7C 9C 8C 5C")))
      (is (= [14, 13, 12, 11 , 10] (p/card-ranks "TC JC QC KC AC")))
      (is (= [10, 9, 8, 7, 6] (p/card-ranks sf)))
      (is (= [9, 9, 9, 9, 7] (p/card-ranks fk)))
      (is (= [10, 10, 10, 7 , 7] (p/card-ranks fh)))
      (is (true? (p/flush sf)))
      (is (false? (p/flush fk)))
      (is (true? (p/straight [9, 8, 7, 6, 5])))
      (is (true? (p/straight (p/card-ranks s1))))
      (is (false? (p/straight [9, 8, 8, 6, 5])))
      (is (= 9 (p/kind 4 (p/card-ranks fk))))
      (is (= nil (p/kind 3 (p/card-ranks fk))))
      (is (= nil (p/kind 2 (p/card-ranks fk))))
      (is (= 7 (p/kind 1 (p/card-ranks fk))))
      (is (nil? (p/two-pair (p/card-ranks fk))))
      (is (= '(9 5) (p/two-pair (p/card-ranks tp)))))))

(deftest count-test
  (testing "count the frequency of a given element in a sequence"
    (is (= 4 (p/count-freq [9 9 9 9 4] 9)))
    (is (= 1 (p/count-freq [9 9 9 9 4] 4)))))
