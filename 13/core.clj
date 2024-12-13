(ns core
  (:require [clojure.string :as str]))

(defn read-buttons-and-prizes [machine-input]
  (->> (re-find #"Button A: X\+(\d+), Y\+(\d+)\nButton B: X\+(\d+), Y\+(\d+)\nPrize: X=(\d+), Y=(\d+)" machine-input)
       (drop 1)
       (map parse-long)))

(defn examine-machines [input]
  (str/split input #"\n\n"))

(defn cramer's-rule
  "https://en.wikipedia.org/wiki/Cramer%27s_rule#Explicit_formulas_for_small_systems"
  [[a1 a2 b1 b2 c1 c2]]
  (let [common-denominator (- (* a1 b2) (* b1 a2))
        x (/ (- (* c1 b2) (* b1 c2)) common-denominator)
        y (/ (- (* a1 c2) (* c1 a2)) common-denominator)]
    (when (and (int? x) (int? y))
      [x y])))

(defn part-1 []
  (->> (examine-machines (slurp "13/input.txt"))
       (keep (comp cramer's-rule read-buttons-and-prizes))
       (reduce #(+ %1 (* 3 (first %2)) (last %2)) 0)))
