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
