(ns poker.core-test
  (:require [clojure.test :refer :all]
            [poker.core :as p]
            [clojure.string :as string]))

(defn poker-string->vec
  "creates a vector from given string"
  [s]
  (string/split s #"\s+"))

(deftest card-ranks-test
  (testing "testing poker functions....."
    (let [sf (poker-string->vec "6C 7C 8C 9C TC")
          sf-best (poker-string->vec "TC JC QC KC AC")
          fk (poker-string->vec "9D 9H 9S 9C 7D")
          fh (poker-string->vec "TD TC TH 7C 7D")
          tp (poker-string->vec "5S 5D 9H 9C 6S")
          s1 (poker-string->vec "AS 2S 3S 4S 5S")
          tk (poker-string->vec "9D 9H 9C 7D 8D")
          flush (poker-string->vec "2S 4S 6S 8S QS")
          straight (poker-string->vec "2S 3S 4S 5S 6D")
          op (poker-string->vec "2S 2D AS 4D 5S")
          hk (poker-string->vec "2S 7D AS 4D 5S")]
      (testing "card-rank function"
        (is (= [10, 9, 8, 7, 6] (p/card-ranks sf)))
        (is (= [14, 13, 12, 11 , 10] (p/card-ranks sf-best)))
        (is (= [10, 9, 8, 7, 6] (p/card-ranks sf)))
        (is (= [9, 9, 9, 9, 7] (p/card-ranks fk)))
        (is (= [10, 10, 10, 7 , 7] (p/card-ranks fh))))
      (testing "helper functions"
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
        (is (= '(9 5) (p/two-pair (p/card-ranks tp)))))
      (testing "hand-rank tests"
        (is (= [7 9 7] (p/hand-rank fk)))
        (is (= [8 10] (p/hand-rank sf)))
        (is (= [6 10 7] (p/hand-rank fh)))
        (is (= [5 '(12 8 6 4 2)] (p/hand-rank flush)))
        (is (= [4, 6] (p/hand-rank straight)))
        (is (= [3 9 '(9 9 9 8 7)] (p/hand-rank tk)))
        (is (= [2 '(9 5) '(9 9 6 5 5)] (p/hand-rank tp)))
        (is (= [1 2 '(14 5 4 2 2)] (p/hand-rank op)))
        (is (= [0 '(14 7 5 4 2)]) (p/hand-rank hk)))
      (testing "allmax tests"
        (is (= [tp]) (p/allmax [hk tp]))
        (is (= [tp tp] (p/allmax [hk hk tp tp]))))
      (testing "poker"
        (is (= [sf] (p/poker [sf fk op hk])))
        (is (= [fk fk] (p/poker [fk fk tp fh])))))))


(deftest count-test
  (testing "count the frequency of a given element in a sequence"
    (is (= 4 (p/count-freq [9 9 9 9 4] 9)))
    (is (= 1 (p/count-freq [9 9 9 9 4] 4)))))
