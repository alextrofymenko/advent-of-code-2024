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
