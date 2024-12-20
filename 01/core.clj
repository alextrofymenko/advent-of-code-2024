(ns core
  (:require
   [clojure.string :as str]))

(defn read-lists []
  (reduce
   (fn [[list-1 list-2] line]
     (let [[item-1 item-2] (map parse-long (str/split line #"\s+"))]
       [(conj list-1 item-1) (conj list-2 item-2)]))
   [] (str/split-lines (slurp "00/input.txt"))))

(defn part-1 []
  (let [[list-1 list-2] (read-lists)]
    (apply + (map #(if (> %1 %2) (- %1 %2) (- %2 %1)) (sort list-1) (sort list-2)))))

(defn part-2 []
  (let [[list-1 list-2] (read-lists)
        freq-2 (frequencies list-2)]
    (reduce
     #(+ %1 (if-let [f (get freq-2 %2)]
              (* %2 f)
              0))
     0 list-1)))
