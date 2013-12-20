(ns poker.core
  (:require (clojure [string :as string]))
  (:gen-class))

(def rank-values "--23456789TJQKA")

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

(defn card-ranks
  "Return a list of the ranks, sorted with the highest first for the given hand.
   e.g.:
   '6C 7C 8C 9C TC' is a straight flush, so the corresponding tuple is:
    (10, 9, 8, 7, 6)"
  [^String hand]
  (sort > (vec (for [x (string/split hand #"\s+")
                   :let [y (subs x 0 1)]]
               (.indexOf rank-values y)))))


(defn straight
  "Return true if the ordered ranks form a 5-card straight"
  [ranks]
  (and (= 5 (count (set ranks))) 
       (= 4 (- (apply max ranks) (apply min ranks)))))


(defn flush
  "Return true if all the cards have the same suit."
  [hand]
  (let [hand-list (string/split hand #"\s+")]
    (if (= 1 (count (reduce #(conj % (second %2)) #{} hand-list)))
      true
      false)))

(defn kind
  "Return the first rank that this hand has exactly n of. Return
   None if there is no n-of-a-kind in the hand."
  [n ranks]
  '9)
