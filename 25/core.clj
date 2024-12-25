(ns core
  (:require
   [clojure.string :as str]))

(defn decode-schematic [s]
  (let [lines (str/split-lines s)
        lock? (every? #(= \# %) (first lines))
        column-heights (map-indexed (fn [i _] (count (filter #(= \# (nth % i)) lines))) (first lines))]
    {(if lock? :locks :keys) column-heights}))

(defn read-schematics [input]
  (reduce
   #(merge-with conj %1 (decode-schematic %2))
   {:locks [] :keys []} (str/split input #"\n\n")))

(defn part-1 []
  (let [schematics (read-schematics (slurp "25/input.txt"))]
    (count
     (for [lock (:locks schematics)
           key (:keys schematics)
           :when (every? #(<= % 7) (map + lock key))]
       [lock key]))))
