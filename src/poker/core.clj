(ns poker.core
  (:refer-clojure :exclude [flush])
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
  (let [ranks (sort > (vec (for [x (string/split hand #"\s+")
                                 :let [y (subs x 0 1)]]
                             (.indexOf rank-values y))))]
    (if (= ranks '(14 5 4 3 2))
      '(5 4 3 2 1)
      ranks)))


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

(defn count-freq
  "Count the frequency of a given element in a sequence"
  [coll elem]
  (reduce #(if (= %2 elem)
             (inc %)
             %) 0 coll))

(defn kind
  "Return the first rank that this hand has exactly n of. Return
   None if there is no n-of-a-kind in the hand."
  [n ranks]
  (first (filter (fn [x] (= n (count-freq ranks x))) ranks)))

(defn two-pair
  "If there are two pair, return thw two ranks as a tuple: 
   (highest, lowest); otherwise return Nil."
  [ranks]
  (let [pairs (reduce (fn [x y]
                        (if (= 2 (count-freq ranks y))
                          (conj x y)
                           x)) 
                      #{}
                      ranks)]
    (if (= 2 (count pairs))
      (sort > pairs)
      nil)))

(defn hand-rank
  "Return a value indicating hhow high the hand ranks."
  [^String hand]
  (let [ranks (card-ranks hand)]
    (cond 
     (and (straight ranks) (flush hand)) (-> [] (conj 8) (conj (apply max ranks)))
     (kind 4 ranks) (-> [] (conj 7) (conj (kind 4 ranks)) (conj (kind 1 ranks))))))

(defn allmax
  "Return a list of all items equals to the max of the sequence."
  [coll key]
  (apply max-key key coll))
