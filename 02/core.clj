(ns core
  (:require
   [clojure.string :as str]))

(defn safe? [values]
  (let [diffs (map (fn [[v1 v2]] (- v1 v2)) (partition 2 1 values))]
    (or (every? #(and (pos? %) (<= 1 % 3)) diffs)
        (every? #(and (neg? %) (>= -1 % -3)) diffs))))

(defn filter-reports [match-fn]
  (for [report (str/split-lines (slurp "02/input.txt"))
        :let [values (map parse-long (str/split report #"\s+"))]
        :when (match-fn values)]
    report))

(defn part-1 []
  (count (filter-reports safe?)))

(defn safe-enough? [values]
  (some true?
   (for [i (range (count values))
         :let [[before after] (split-at i values)
               values* (concat before (drop 1 after))]]
     (safe? values*))))

(defn part-2 []
  (count (filter-reports #(or (safe? %) (safe-enough? %)))))
