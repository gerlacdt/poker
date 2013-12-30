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
  [hand]
  (let [ranks (sort > (for [x hand
                                :let [y (subs x 0 1)]]
                            (.indexOf rank-values y)))]
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
  (if (= 1 (count (reduce #(conj % (second %2)) #{} hand)))
    true
    false))

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
  "If there are two pair, return the two ranks as a tuple: 
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
  "Return a value indicating how high the hand ranks."
  [hand]
  (let [ranks (card-ranks hand)]
    (cond 
     (and (straight ranks) (flush hand)) (-> [] (conj 8) (conj (apply max ranks)))
     (kind 4 ranks) (-> [] (conj 7) (conj (kind 4 ranks)) (conj (kind 1 ranks)))
     (and (kind 3 ranks) (kind 2 ranks)) (-> [] (conj 6) (conj (kind 3 ranks)) (conj (kind 2 ranks)))
     (flush hand) (-> [] (conj 5) (conj ranks))
     (straight ranks) (conj [4] (apply max ranks))
     (kind 3 ranks) (-> [3] (conj (kind 3 ranks)) (conj ranks))
     (two-pair ranks) (-> [2] (conj (two-pair ranks)) (conj ranks))
     (kind 2 ranks) (-> [1] (conj (kind 2 ranks)) (conj ranks))
     :else (-> [0] (conj ranks))
)))

(defn find-max-poker
  "Return the max hand of the given poker hands."
  [hands]
  (let [min-count (count (apply min-key count (for [hand hands]
                                                (hand-rank hand))))]
    (reduce (fn [x y] 
              (if (<= 0 (compare (subvec (vec (flatten (hand-rank x))) 0 min-count) 
                                 (subvec (vec (flatten (hand-rank y))) 0 min-count)))
                x
                y)) hands)))

(defn allmax
  "Return a list of all items equals to the max of the sequence."
  [coll]
  (let [maximum (find-max-poker coll)]
    (for [x coll :when (= maximum x)]
      x)))

(defn poker
  "Return a list of winning hands. poker([hand1, hand2, ...] => [hand, ..."
  [hands]
  (allmax hands))

(def deck (for [r "23456789TJQKA" s "SHDC"]
            (str r s)))

(defn deal
  "Shuffle the deck and deal out numhands n-card hands.
   e.g. deal 4 5 => 4 players with 5 cards"
  [numhands n]
  (let [shuffled-deck (shuffle deck)]
    (for [i (range numhands)]
      (subvec shuffled-deck (* n i) (* n (+ i 1))))))

(def hand-names ["high card" "one pair" "two pair" "three-kind" "straight" "flush" "full house" "four kind" "straight flush"])

(defn hand-percentages
  "Sample n random hands and print a table of percentages for each
  type of hand. n should be 700,000"  
  [n]
  (let [counts (atom (vec (repeat 9 0)))]
    (dotimes [_ (/ n 10)]
      (doseq [hand (deal 10 5)]
        (let [rank (first (hand-rank hand))]
          (swap! counts assoc rank (+ 1 (nth @counts rank))))))
    (dotimes [i 9]
      (println (format "%14s: %6.3f %%" (nth hand-names i) (float (* 100 (/ (nth @counts i) n))))))
    (println @counts)))
